type vyconf_defaults = {
    config_file: string;
    pid_file: string;
    socket: string;
    log_template: string;
    log_level: string;
    legacy_config_path: string;
}

let defaults = {
    config_file = "/etc/vyos/vyconfd.conf";
    pid_file = "/var/run/vyconfd.pid";
    socket = "/var/run/vyconfd.sock";
    log_template = "$(date) $(name)[$(pid)]: $(message)";
    log_level = "notice";
    legacy_config_path = "/opt/vyatta/etc/config/config.boot";
}
