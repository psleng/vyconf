module CT = Vyos1x.Config_tree
module IC = Vyos1x.Internal.Make(CT)
module CC = Commitd_client.Commit
module CD = Vyos1x.Config_diff
module VT = Vyos1x.Vytree
module RT = Vyos1x.Reference_tree
module D = Directories
module FP = FilePath

exception Session_error of string

type cfg_op =
    | CfgSet of string list * string option * CT.value_behaviour
    | CfgDelete of string list * string	option

type world = {
    mutable running_config: CT.t;
    mutable reference_tree: RT.t;
    vyconf_config: Vyconf_config.t;
    dirs: Directories.t
}

type session_data = {
    proposed_config : CT.t;
    modified: bool;
    conf_mode: bool;
    changeset: cfg_op list;
    client_app: string;
    user: string;
    client_pid: int32;
}

let make world client_app user pid = {
    proposed_config = world.running_config;
    modified = false;
    conf_mode = false;
    changeset = [];
    client_app = client_app;
    user = user;
    client_pid = pid;
}

let string_of_op op =
    match op with
    | CfgSet (path, value, _) ->
        let path_str = Vyos1x.Util.string_of_list path in
        (match value with
         | None -> Printf.sprintf "set %s" path_str
         | Some v -> Printf.sprintf "set %s \"%s\"" path_str v)
    | CfgDelete (path, value) ->
        let path_str = Vyos1x.Util.string_of_list path in
        (match value with
         | None -> Printf.sprintf "delete %s" path_str
         | Some v -> Printf.sprintf "delete %s \"%s\"" path_str v)


let set_modified s =
    if s.modified = true then s
    else {s with modified = true}

let apply_cfg_op op config =
    match op with
    | CfgSet (path, value, value_behaviour) ->
        CT.set config path value value_behaviour
    | CfgDelete (path, value) -> 
        CT.delete config path value

let rec apply_changes changeset config =
    match changeset with
    | [] -> config
    | c :: cs -> apply_changes cs (apply_cfg_op c config)

let validate w _s path =
    try
        RT.validate_path D.(w.dirs.validators) w.reference_tree path
    with RT.Validation_error x -> raise (Session_error x)

let validate_tree w' t =
    let validate_path w out path =
        let res =
            try
                RT.validate_path D.(w.dirs.validators) w.reference_tree path;
                out
            with RT.Validation_error x -> out ^ x
        in res
    in
    let paths = CT.value_paths_of_tree t in
    let out = List.fold_left (validate_path w') "" paths in
    match out with
    | "" -> ()
    | _ -> raise (Session_error out)

let split_path w _s path =
    RT.split_path w.reference_tree path

let set w s path =
    let _ = validate w s path in
    let path, value = split_path w s path in
    let refpath = RT.refpath w.reference_tree path in
    let value_behaviour = if RT.is_multi w.reference_tree refpath then CT.AddValue else CT.ReplaceValue in
    let op = CfgSet (path, value, value_behaviour) in
    let config =
        try
            apply_cfg_op op s.proposed_config |>
            (fun c -> RT.set_tag_data w.reference_tree c path) |>
            (fun c -> RT.set_leaf_data w.reference_tree c path)
        with
        | CT.Useless_set | CT.Duplicate_value -> s.proposed_config

    in
    {s with proposed_config=config; changeset=(op :: s.changeset)}

let prune_del_path node path =
    if CT.is_tag_value node path then
        let tag_path = Vyos1x.Util.drop_last path in
        let terminal = VT.is_terminal_path node tag_path in
        match terminal with
        | true -> CT.delete node tag_path None
        | false -> node
    else node

let delete w s path =
    let path, value = split_path w s path in
    let op = CfgDelete (path, value) in
    let config =
        try
            apply_cfg_op op s.proposed_config |>
            (fun c -> prune_del_path c path)
        with
        | VT.Nonexistent_path | CT.No_such_value -> s.proposed_config
    in
    {s with proposed_config=config; changeset=(op :: s.changeset)}

let discard w s =
    {s with proposed_config=w.running_config}

let session_changed w s =
    (* structural equality test requires consistent ordering, which is
     * practised, but may be unreliable; test actual difference
     *)
    let diff = CD.diff_tree [] w.running_config s.proposed_config in
    let add_tree = CT.get_subtree diff ["add"] in
    let del_tree = CT.get_subtree diff ["del"] in
    (del_tree <> CT.default) || (add_tree <> CT.default)

let load w s file cached =
    let ct =
        if cached then
            try
                Ok (IC.read_internal file)
            with Vyos1x.Internal.Read_error e ->
                Error e
        else
            Vyos1x.Config_file.load_config file
    in
    match ct with
    | Error e -> raise (Session_error (Printf.sprintf "Error loading config: %s" e))
    | Ok config ->
        validate_tree w config; {s with proposed_config=config;}

let merge w s file destructive =
    let ct = Vyos1x.Config_file.load_config file in
    match ct with
    | Error e -> raise (Session_error (Printf.sprintf "Error loading config: %s" e))
    | Ok config ->
        let () = validate_tree w config in
        let merged = CD.tree_merge ~destructive:destructive s.proposed_config config
        in
        {s with proposed_config=merged;}

let save w s file =
    let ct = w.running_config in
    let res = Vyos1x.Config_file.save_config ct file in
    match res with
    | Error e -> raise (Session_error (Printf.sprintf "Error saving config: %s" e))
    | Ok () -> s

let prepare_commit ?(dry_run=false) w s id =
    let at = w.running_config in
    let wt = s.proposed_config in
    let rt = w.reference_tree in
    let vc = w.vyconf_config in
    let () =
        try
            IC.write_internal at (FP.concat vc.session_dir vc.running_cache)
        with
            Vyos1x.Internal.Write_error msg -> raise (Session_error msg)
    in
    let () =
        try
            IC.write_internal wt (FP.concat vc.session_dir vc.session_cache)
        with
            Vyos1x.Internal.Write_error msg -> raise (Session_error msg)
    in
    CC.make_commit_data ~dry_run:dry_run rt at wt id

let get_config w s id =
    let at = w.running_config in
    let wt = s.proposed_config in
    let vc = w.vyconf_config in
    let running_cache = Printf.sprintf "%s_%s" vc.running_cache id in
    let session_cache = Printf.sprintf "%s_%s" vc.session_cache id in
    let () =
        try
            IC.write_internal at (FP.concat vc.session_dir running_cache)
        with
            Vyos1x.Internal.Write_error msg -> raise (Session_error msg)
    in
    let () =
        try
            IC.write_internal wt (FP.concat vc.session_dir session_cache)
        with
            Vyos1x.Internal.Write_error msg -> raise (Session_error msg)
    in id

let cleanup_config w id =
    let remove_file file =
        if Sys.file_exists file then
            Sys.remove file
    in
    let vc = w.vyconf_config in
    let running_cache = Printf.sprintf "%s_%s" vc.running_cache id in
    let session_cache = Printf.sprintf "%s_%s" vc.session_cache id in
    remove_file (FP.concat vc.session_dir running_cache);
    remove_file (FP.concat vc.session_dir session_cache)

let get_value w s path =
    if not (VT.exists s.proposed_config path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if not (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot get a value of a non-leaf node")
    else if (RT.is_multi w.reference_tree refpath) then
        raise (Session_error "This node can have more than one value")
    else if (RT.is_valueless w.reference_tree refpath) then
        raise (Session_error "This node can have more than one value")
    else CT.get_value s.proposed_config path

let get_values w s path =
    if not (VT.exists s.proposed_config path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if not (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot get a value of a non-leaf node")
    else if not (RT.is_multi w.reference_tree refpath) then
        raise (Session_error "This node can have only one value")
    else CT.get_values s.proposed_config path

let list_children w s path =
    if not (VT.exists s.proposed_config path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot list children of a leaf node")
    else VT.children_of_path s.proposed_config path

let exists _w s path =
    VT.exists s.proposed_config path

let show_config _w s path fmt =
    let open Vyconf_connect.Vyconf_pbt in
    if (path <> []) && not (VT.exists s.proposed_config path) then
        raise (Session_error ("Path does not exist")) 
    else
        let node = s.proposed_config in
        match fmt with
        | Curly -> CT.render_at_level node path
        | Json ->
            let node =
                (match path with [] -> s.proposed_config |
                                 _ as ps -> VT.get s.proposed_config ps) in
            CT.to_yojson node |> Yojson.Safe.pretty_to_string
