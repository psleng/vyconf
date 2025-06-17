open Vyconfd_client.Vyconf_client
open Vyconf_connect.Vyconf_pbt

type op_t =
    | OpSet
    | OpDelete
    | OpDiscard
    | OpShowConfig
    | OpSessionChanged

let op_of_string s =
    match s with
    | "vy_set" -> OpSet
    | "vy_delete" -> OpDelete
    | "vy_discard" -> OpDiscard
    | "vy_show" -> OpShowConfig
    | "vy_session_changed" -> OpSessionChanged
    | _ -> failwith (Printf.sprintf "Unknown operation %s" s)

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
    let pid = Int32.of_int (Unix.getppid()) in
    let socket = "/var/run/vyconfd.sock" in
    let config_format = config_format_of_string "curly" in
    let out_format = output_format_of_string "plain" in
    let%lwt client =
        create socket out_format config_format
    in
    let%lwt resp = session_of_pid client pid in
    match resp with
    | Error _ -> setup_session client "vyconf_cli" pid
    | _ as c -> c |> Lwt.return

let main op path =
    let%lwt client = get_session () in
    let%lwt result =
    match client with
    | Ok c ->
        begin
        match op with
        | OpSet -> set c path
        | OpDelete -> delete c path
        | OpDiscard -> discard c
        | OpShowConfig -> show_config c path
        | OpSessionChanged -> session_changed c
        end
    | Error e -> Error e |> Lwt.return
    in
    match result with
    | Ok s -> let%lwt () = Lwt_io.write Lwt_io.stdout s in Lwt.return 0
    | Error e -> let%lwt () = Lwt_io.write Lwt_io.stderr (Printf.sprintf "%s\n" e) in Lwt.return 1

let () =
    let path_list = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
    in
    let op_str = FilePath.basename Sys.argv.(0) in
    let op = op_of_string op_str in
    let result = Lwt_main.run (main op path_list) in exit result
