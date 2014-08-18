%%%---------------------------------------------------------------------------

Header
  "%%% @private"
.

Nonterminals
  config
  option_list
  section_list one_section
.

Terminals
  option section
.

Rootsymbol config.

%%%---------------------------------------------------------------------------

config -> option_list section_list : {lists:reverse('$1'), lists:reverse('$2')}.

option_list -> option_list option : [value('$2') | '$1'].
option_list -> '$empty' : [].

section_list -> section_list one_section : ['$2' | '$1'].
section_list -> '$empty' : [].

one_section -> section option_list : {value('$1'), lists:reverse('$2')}.

%%%---------------------------------------------------------------------------

Erlang code.

%terminal({TermName, _Line}) ->
%  TermName;
%terminal({TermName, _Line, _Value}) ->
%  TermName.

value({_TermName, _Line, Value}) ->
  Value.

%%%---------------------------------------------------------------------------
%%% vim:ft=erlang
