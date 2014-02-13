%%%---------------------------------------------------------------------------
%%% @doc
%%%   INI-like config loader.
%%% @end
%%%---------------------------------------------------------------------------

-module(eni).

-export([file/1, string/1]).

%%%---------------------------------------------------------------------------
%%% types

%% @type config() = {NoSectionOptions :: [option()], Sections :: [section()]}.

-type config() :: {[option()], [section()]}.

%% @type section() = {section_name(), [option()]}.

-type section() :: {section_name(), [option()]}.

%% @type section_name() = atom().

-type section_name() :: atom().

%% @type option() = {Name :: atom(), Value :: term()}.

-type option() :: {atom(), term()}.

%%%---------------------------------------------------------------------------

%% @doc Load configuration from file.
%% @spec file(string()) -> config() | {error,Reason}

-spec file(string()) -> config() | {error,term()}.

file(File) ->
  case file:read_file(File) of
    {ok, Content} ->
      string(Content);
    {error, _Reason} = Error ->
      Error
  end.

%% @doc Load configuration from string.
%% @spec string(string()) -> config() | {error,Reason}

-spec string(string() | binary()) -> config() | {error,term()}.

string(String) when is_binary(String) ->
  string(binary_to_list(String));
string(String) when is_list(String) ->
  case eni_lexer:string(String) of
    {ok, Tokens, _EOFLineNum} ->
      case eni_parser:parse(Tokens) of
        {ok, {_NoSectionOpts, _Sections}} = Result ->
          Result;
        {error, _Reason} = Error ->
          Error
      end;
    {error, Reason, _LineNum} ->
      {error, Reason}
  end.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang:foldmethod=marker
