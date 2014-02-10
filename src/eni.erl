%%%---------------------------------------------------------------------------
%%% @doc
%%%   INI file parser.
%%% @end
%%%---------------------------------------------------------------------------

-module(eni).

-export([file/1, string/1]).

%%%---------------------------------------------------------------------------
%%% types

%% @type config() = {option_list(), [section()]}.

-type config() :: {option_list(), [section()]}.

%% @type section() = {section_name(), option_list()}.

-type section() :: {section_name(), option_list()}.

%% @type section_name() = atom().

-type section_name() :: atom().

%% @type option_list() = [{atom(), term()}].

-type option_list() :: [{atom(), term()}].

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
