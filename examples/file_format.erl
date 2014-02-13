%%%---------------------------------------------------------------------------
%%% @doc
%%%   ENI file format description.
%%%
%%%   The format is derived from DOS/Windows INI file, but extended a little
%%%   to support Erlang terms.
%%%
%%%   ENI file consists of options, each in separate line, grouped in
%%%   sections. Begin of a section is denoted by square brackets
%%%   (`[section_name]'). The options before the first section marker are
%%%   returned as a separate.
%%%
%%%   Option name may contain any combination of letters (`[a-zA-Z]'), digits,
%%%   period, underscore or hyphen. The same stands for section name.
%%%
%%%   An example config (note "`:='" and "`='" option styles):
%%%   ```
%%%   ; traditional INI comment
%%%   # more "unixish" comment
%%%   % and "erlangish" comment
%%%
%%%   pid_file = /var/run/erl_daemon.pid
%%%   enabled_services := [http, https].
%%%
%%%   [http]
%%%   bind = 127.0.0.1
%%%   port := [8080, 8880].
%%%
%%%   [https]
%%%   bind := any.
%%%   port = 8443
%%%   ssl_cert = /etc/erl_daemon/cert.pem
%%%   ssl_key  = /etc/erl_daemon/key.pem
%%%   '''
%%%
%%%   === String options ===
%%%
%%%   Option specified with "`='" character is considered a (possibly empty)
%%%   string. Such string has leading and trailing spaces stripped, but spaces
%%%   in the middle of the value are preserved.
%%%
%%%   In Erlang they are represented as normal strings (that is, as lists).
%%%
%%%   === Erlang term options ===
%%%
%%%   Option specified with "`:='" has Erlang term as the value. Such term
%%%   may, but need not to, be ended with a period. The term needs to fit in
%%%   a signle line, but no constraints on the line length is enforced.
%%%
%%%   Fitting in a single line requirement may be relaxed in the future.
%%%
%%%   Considering the example above, there are three options of this flavour:
%%%   <i>enabled_services</i> (containing a list of two atoms), <i>port</i>
%%%   from <i>http</i> section (list of two integers) and <i>bind</i> from
%%%   <i>https</i> section (single atom `any').
%%%
%%%   @TODO Add support for including a file or directory.
%%%
%%% @end
%%%---------------------------------------------------------------------------

-module(file_format).
