Definitions.

ID = [a-zA-Z0-9._-]+
S  = [\s\t]*
NL = \r?\n

%%%---------------------------------------------------------------------------

Rules.

% comment
{S}([%#;].*)?{NL} : skip_token.

% section header
{S}\[{ID}\]{S}{NL} : {token, {section, TokenLine, section(TokenChars)}}.

% regular option (string = string)
{S}{ID}{S}={S}.* : {token, {option,  TokenLine, option(TokenChars)}}.
% Erlang term option, ended with period
{S}{ID}{S}:={S}.*\.{S} :
  case option(TokenChars) of
    {ok, Value} ->
      {token, {option, TokenLine, Value}};
    {error, _Reason} ->
      {error, "Invalid token"}
  end.
% Erlang term option, not ended with period
{S}{ID}{S}:={S}.* :
  case option(TokenChars ++ ".") of
    {ok, Value} ->
      {token, {option, TokenLine, Value}};
    {error, _Reason} ->
      {error, "Invalid token"}
  end.

{NL} : skip_token.

%%%---------------------------------------------------------------------------

Erlang code.

% I'm sure it will start with "[" and will contain "]"
section("[" ++ String) ->
  P = string:chr(String, $]),
  list_to_atom(string:substr(String, 1, P - 1)).

option(String) ->
  try
    {Name, Value} = split(String),
    {ok, {list_to_atom(Name), Value}}
  catch
    % TODO: better error reporting
    _:_ -> {error, badarg}
  end.

split("=" ++ Value) ->
  {"", string:strip(Value)}; % TODO: strip "\t" and "\r"
split(":=" ++ Value) ->
  % TODO: catch errors here?
  {ok, Tokens, _LineNo} = erl_scan:string(Value),
  {ok, Term} = erl_parse:parse_term(Tokens),
  {"", Term};
split([C | String]) when C == $  orelse C == $\t ->
  split(String); % skip
split([C | String]) ->
  {ID, Value} = split(String),
  {[C | ID], Value}.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang
