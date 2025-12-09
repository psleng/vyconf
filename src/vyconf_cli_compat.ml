open Vyconfd_client.Vyconf_client
open Vyconf_connect.Vyconf_pbt

type op_t =
    | OpSetEditLevel
    | OpSetEditLevelUp
    | OpResetEditLevel
    | OpGetEditLevel
    | OpEditLevelRoot
    | OpShowConfig
    | OpSessionChanged
    | OpConfigUnsaved
    | OpReferencePathExists

let op_of_arg s =
    match s with
    | "getEditEnv" -> OpSetEditLevel
    | "getEditUpEnv" -> OpSetEditLevelUp
    | "getEditResetEnv" -> OpResetEditLevel
    | "getEditLevelStr" -> OpGetEditLevel
    | "editLevelAtRoot" -> OpEditLevelRoot
    | "showCfg" -> OpShowConfig
    | "sessionChanged" -> OpSessionChanged
    | "sessionUnsaved" -> OpConfigUnsaved
    | "validateTmplPath" -> OpReferencePathExists
    | _ -> failwith (Printf.sprintf "Unknown operation %s" s)

let in_cli_config_session () =
    let env = Unix.environment () in
    let res = Array.find_opt (fun c -> String.starts_with ~prefix:"_OFR_CONFIGURE" c) env
    in
    match res with
    | Some _ -> true
    | None -> false

let config_format_of_string s =
    match s with
    | "curly" -> Curly
    | "json" -> Json
    | _ -> failwith (Printf.sprintf "Unknown config format %s, should be curly or json" s)

let output_format_of_string s =
    match s with
    | "plain" -> Out_plain
    | "json" ->	Out_json
    | _	-> failwith (Printf.sprintf "Unknown output format %s, should be plain or json" s)

let get_session () =
    let pid =
        try (* set for config session *)
            Int32.of_string (Sys.getenv "SESSION_PID")
        with Not_found ->
            Int32.of_int (Unix.getppid())
    in
    let user =
        try Sys.getenv "USER"
        with Not_found -> ""
    in
    let sudo_user =
        try Sys.getenv "SUDO_USER"
        with Not_found -> ""
    in
    let socket = "/var/run/vyconfd.sock" in
    let config_format = config_format_of_string "curly" in
    let out_format = output_format_of_string "plain" in
    let%lwt client =
        create socket out_format config_format
    in
    let%lwt resp = session_of_pid client pid in
    match resp with
    | Error _ -> setup_session client "vyconf_cli_compat" sudo_user user pid
    | _ as c -> c |> Lwt.return

let close_session () =
    let%lwt client = get_session () in
    match client with
    | Ok c ->
        teardown_session c
    | Error e -> Error e |> Lwt.return

let main op path =
    let%lwt client = get_session () in
    let%lwt result =
    match client with
    | Ok c ->
        begin
        match op with
        | OpSetEditLevel -> set_edit_level c path
        | OpSetEditLevelUp -> set_edit_level_up c
        | OpResetEditLevel -> reset_edit_level c
        | OpGetEditLevel -> get_edit_level c
        | OpEditLevelRoot -> edit_level_root c
        | OpShowConfig -> show_config c path
        | OpSessionChanged -> session_changed c
        | OpConfigUnsaved -> config_unsaved c None
        | OpReferencePathExists -> reference_path_exists c path
        end
    | Error e -> Error e |> Lwt.return
    in
    let () =
        if not (in_cli_config_session ()) then
            close_session () |> Lwt.ignore_result
    in
    match result with
    | Ok s ->
        begin
        match s with
        | "" -> Lwt.return 0
        | _ ->
        let%lwt () =
            Lwt_io.write Lwt_io.stdout (Printf.sprintf "%s\n" s) in Lwt.return 0
        end
    | Error e ->
        begin
        match e with
        | "" -> Lwt.return 1
        | _ ->
        let%lwt () =
            Lwt_io.write Lwt_io.stderr (Printf.sprintf "%s\n" e) in Lwt.return 1
        end

let () =
    if (Array.length Sys.argv) < 2 then
        let () = print_endline "Must specify operation" in exit 1
    else
    let path_list = Array.to_list (Array.sub Sys.argv 2 (Array.length Sys.argv - 2))
    in
    let op =
        try
            op_of_arg Sys.argv.(1)
        with Failure msg -> let () = print_endline msg in exit 1
    in
    match op, path_list with
    | OpSetEditLevel, [] | OpReferencePathExists, [] ->
        let () = print_endline "Must specify config path" in exit 1
    | _, _ ->
        let result = Lwt_main.run (main op path_list) in exit result
