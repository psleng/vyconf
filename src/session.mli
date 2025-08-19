type cfg_op =
    | CfgSet of string list * string option * Vyos1x.Config_tree.value_behaviour
    | CfgDelete of string list * string option

type world = {
    mutable running_config: Vyos1x.Config_tree.t;
    mutable reference_tree: Vyos1x.Reference_tree.t;
    vyconf_config: Vyconf_config.t;
    dirs: Directories.t
}

type session_data = {
    proposed_config : Vyos1x.Config_tree.t;
    modified: bool;
    conf_mode: bool;
    changeset: cfg_op list;
    client_app: string;
    user: string;
    client_pid: int32
}

exception Session_error of string

val make : world -> string -> string -> int32 -> session_data

val set_modified : session_data -> session_data

val validate : world -> session_data -> string list -> unit

val get_changeset : world -> Vyos1x.Config_tree.t -> Vyos1x.Config_tree.t -> cfg_op list

val set : world -> session_data -> string list -> session_data

val delete : world -> session_data -> string list -> session_data

val get_proposed_config : world -> session_data -> Vyos1x.Config_tree.t

val discard : world -> session_data -> session_data

val session_changed : world -> session_data -> bool

val load : world -> session_data -> string -> bool -> session_data

val merge : world -> session_data -> string -> bool -> session_data

val save : world -> session_data -> string -> session_data

val get_value : world -> session_data -> string list -> string

val get_values : world -> session_data -> string list -> string list

val exists : world -> session_data -> string list -> bool

val list_children : world -> session_data -> string list -> string list

val string_of_op : cfg_op -> string

val prepare_commit : ?dry_run:bool -> world -> Vyos1x.Config_tree.t -> string -> Commitd_client.Commit.commit_data

val get_config : world -> session_data -> string -> string

val cleanup_config : world -> string -> unit

val show_config : world -> session_data -> string list -> Vyconf_connect.Vyconf_pbt.request_config_format -> string
