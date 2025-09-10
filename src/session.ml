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
    client_pid: int32;
    client_user: string;
    client_sudo_user: string;
}

let make world client_app sudo_user user pid = {
    proposed_config = world.running_config;
    modified = false;
    conf_mode = false;
    changeset = [];
    client_app = client_app;
    client_user = user;
    client_sudo_user = sudo_user;
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

let apply_cfg_op w op config =
    let result =
    match op with
    | CfgSet (path, value, value_behaviour) ->
        begin
        let rt = w.reference_tree in
        let refp = RT.refpath rt path in
        try
            let c =
            match (RT.is_leaf rt refp) with
            | true ->
                CT.set config path value value_behaviour
            | false ->
                CT.create_node config path
            in
            RT.set_tag_data rt c path
        with
        | CT.Useless_set | CT.Duplicate_value -> config
        end
    | CfgDelete (path, value) -> 
        begin
        try
            CT.delete config path value |>
            (fun c -> CT.prune_delete c path)
        with
        | VT.Nonexistent_path | CT.No_such_value -> config
        end
    in result

let rec apply_changes w changeset config =
    match changeset with
    | [] -> config
    | c :: cs -> apply_changes w cs (apply_cfg_op w c config)

let validate w _s path =
    try
        RT.validate_path D.(w.dirs.validators) w.reference_tree path
    with RT.Validation_error x -> raise (Session_error x)

let validate_tree w t =
    let out = RT.validate_tree D.(w.dirs.validators) w.reference_tree t in
    match out with
    | "" -> ()
    | _ -> raise (Session_error out)

let split_path w path =
    RT.split_path w.reference_tree path

let get_proposed_config w s =
    let c = w.running_config in
    apply_changes w (List.rev s.changeset) c

let update_set w changeset path =
    let path, value = split_path w path in
    let refpath = RT.refpath w.reference_tree path in
    let value_behaviour = if RT.is_multi w.reference_tree refpath then CT.AddValue else CT.ReplaceValue in
    let op = CfgSet (path, value, value_behaviour) in
    (op :: changeset)

let update_delete w changeset path =
    let path, value = split_path w path in
    let op = CfgDelete (path, value) in
    (op :: changeset)

let get_changeset w lt rt =
    let diff = CD.diff_tree [] lt rt in
    let add_tree = CT.get_subtree diff ["add"] in
    let del_tree = CT.get_subtree diff ["del"] in
    let add_changeset =
        List.fold_left (update_set w) [] (CT.value_paths_of_tree add_tree)
    in
    let del_changeset =
        List.fold_left (update_delete w) [] (CT.value_paths_of_tree del_tree)
    in
    add_changeset @ del_changeset

let set w s path =
    let _ = validate w s path in
    let changeset' = update_set w s.changeset path in
    { s with changeset = changeset' }

let delete w s path =
    let changeset' = update_delete w s.changeset path in
    { s with changeset = changeset' }

let discard _w s =
    { s with changeset = []; }

let session_changed w s =
    (* structural equality test requires consistent ordering, which is
     * practised, but may be unreliable; test actual difference
     *)
    let c = get_proposed_config w s in
    let diff = CD.diff_tree [] w.running_config c in
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
        validate_tree w config;
        { s with changeset = get_changeset w w.running_config config; }

let merge w s file destructive =
    let ct = Vyos1x.Config_file.load_config file in
    match ct with
    | Error e -> raise (Session_error (Printf.sprintf "Error loading config: %s" e))
    | Ok config ->
        let () = validate_tree w config in
        let proposed = get_proposed_config w s in
        let merged = CD.tree_merge ~destructive:destructive proposed config
        in
        { s with changeset = get_changeset w w.running_config merged; }

let save w s file =
    let ct = w.running_config in
    let res = Vyos1x.Config_file.save_config ct file in
    match res with
    | Error e -> raise (Session_error (Printf.sprintf "Error saving config: %s" e))
    | Ok () -> s

let prepare_commit ?(dry_run=false) w config id =
    let at = w.running_config in
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
            IC.write_internal config (FP.concat vc.session_dir vc.session_cache)
        with
            Vyos1x.Internal.Write_error msg -> raise (Session_error msg)
    in
    CC.make_commit_data ~dry_run:dry_run rt at config id

let get_config w s id =
    let at = w.running_config in
    let wt = get_proposed_config w s in
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
    let c = get_proposed_config w s in
    if not (VT.exists c path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if not (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot get a value of a non-leaf node")
    else if (RT.is_multi w.reference_tree refpath) then
        raise (Session_error "This node can have more than one value")
    else if (RT.is_valueless w.reference_tree refpath) then
        raise (Session_error "This node can have more than one value")
    else CT.get_value c path

let get_values w s path =
    let c = get_proposed_config w s in
    if not (VT.exists c path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if not (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot get a value of a non-leaf node")
    else if not (RT.is_multi w.reference_tree refpath) then
        raise (Session_error "This node can have only one value")
    else CT.get_values c path

let list_children w s path =
    let c = get_proposed_config w s in
    if not (VT.exists c path) then
        raise (Session_error ("Config path does not exist"))
    else let refpath = RT.refpath w.reference_tree path in
    if (RT.is_leaf w.reference_tree refpath) then
        raise (Session_error "Cannot list children of a leaf node")
    else VT.children_of_path c path

let exists w s path =
    let c = get_proposed_config w s in
    VT.exists c path

let show_config w s path fmt =
    let open Vyconf_connect.Vyconf_pbt in
    let c = get_proposed_config w s in
    if (path <> []) && not (VT.exists c path) then
        raise (Session_error ("Path does not exist")) 
    else
        let node = c in
        match fmt with
        | Curly -> CT.render_at_level node path
        | Json ->
            let node =
                (match path with [] -> c |
                                 _ as ps -> VT.get c ps) in
            CT.to_yojson node |> Yojson.Safe.pretty_to_string
