-module(string_interpolation_test).

-compile({parse_transform, string_interpolation}).

-compile(export_all).

'{'(Atom) ->
	atom_to_list(Atom).
'}'(Atom) ->
	atom_to_list(Atom).
'{}'(Atom) ->
	atom_to_list(Atom).
'#{}'(Atom) ->
	atom_to_list(Atom).

nop(A) ->
	A.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Simple tests.

empty_test()->
	?assertEqual("", lists:flatten('$'("#{}"))).

smallest_test() ->
	A = "test",	
	?assertEqual("test", lists:flatten('$'("#{A}"))).		

start_test() ->
	A = "Test",
	?assertEqual("Test is ok", lists:flatten('$'("#{A} is ok"))).

middle_test() ->
	A = "test",
	?assertEqual("Testing test function", lists:flatten('$'("Testing #{A} function"))).

end_test() ->
	A = "test",
	?assertEqual("This is test", lists:flatten('$'("This is #{A}"))).

multiple_test() ->
	A = "Test",
	B = "ok",
	?assertEqual("Test is ok", lists:flatten('$'("#{A} is #{B}"))).

function_call_test() ->
	A = atom,
	?assertEqual("This is atom", lists:flatten('$'("This is #{atom_to_list(A)}"))).

%% % %% Hard tests.

string_test_() ->
	Tests = [
			 {"\"", '$'("#{\"\\\"\"}")},
			 {"\"\"", '$'("#{\"\\\"\\\"\"}")},
			 {"}", '$'("#{\"}\"}")},
			 {"{", '$'("#{\"{\"}")},
			 {"{}", '$'("#{\"{}\"}")},
			 {"#{}", '$'("#{\"#{}\"}")}
			],
	[fun() -> ?assertEqual(R, lists:flatten(D)) end || {R, D} <- Tests].

string_in_istring_test() ->
	?assertEqual("String is string", lists:flatten('$'("String is #{\"string\"}"))).

string_funarg_in_istring_test() ->
	?assertEqual("String is string", lists:flatten('$'("String is #{nop(\"string\")}"))).

bad_function_name_test_() ->
	A = ok,
	Module = ?MODULE,
	Tests = [{"function {", '$'("All is #{'{'(A)}")},
			 {"function }", '$'("All is #{'}'(A)}")},
			 {"function {}", '$'("All is #{'{}'(A)}")},
			 {"function #{}", '$'("All is #{'#{}'(A)}")},
			 {"remote function", '$'("All is #{Module:'#{}'(A)}")}
			],
	[{T, fun() -> ?assertEqual("All is ok", lists:flatten(R)) end} || {T, R} <- Tests].

istring_in_istring_test() ->
	A = "ok",
	?assertEqual("String is string is ok", lists:flatten('$'("String is #{'$'(\"string is #{A}\")}"))).

string_in_istring_in_string_test() ->
	A = "4",
	?assertEqual("1 2 3 4", lists:flatten('$'("1 #{'$'(\"2 #{'$'(\\\"3 #{A}\\\")}\")}"))).

-endif.
