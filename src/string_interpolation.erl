-module(string_interpolation).

-export([parse_transform/2,
		 pretty_print/1]).

-include("include/ast_helpers.hrl").

-define(DBG(F, D), io:format("~p:~p "++F++"~n", [?FILE, ?LINE | D])).

%% Parse transform.

parse_transform(Ast, _Options)->
	try
		%% ?DBG("~n~p~n=======~n", [Ast]),
		%% ?DBG("~n~s~n=======~n", [pretty_print(Ast)]),
		{Ast2, Errors} = lists:mapfoldl(fun transform_functions/2, [], Ast),
		ErrorsAst = [global_error_ast(L, R) || {L, R} <- Errors],
		Ast3 = ErrorsAst ++ Ast2,
		%% ?DBG("~n~p~n<<<<~n", [Ast3]),
		%% ?DBG("~n~s~n>>>>~n", [pretty_print(Ast3)]),
		Ast3
	catch T:E ->
			Reason = io_lib:format("~p:~p | ~p ~n", [T, E, erlang:get_stacktrace()]),
			[global_error_ast(1, Reason) | Ast]
	end.

transform_functions({function,Line,Name,G,Clauses}, Errors) ->
	{Clauses2, Errors2} = transform_node(Clauses, Errors),
	{{function,Line,Name,G,Clauses2}, Errors2};
transform_functions(Node, Errors) -> {Node, Errors}.

%% Node transform.

transform_node({call, _, {atom, _, '$'}, [{string, Line, String}=StringNode]}, Errors) ->
	case si(String) of
		{ok, Node} ->
			transform_node(Node, Errors);
		{error, Reason} ->
			{StringNode, [{Line, Reason} | Errors]}
	end;
transform_node({clauses, Clauses}, Errors) ->
	{Ast, Errors2} = transform_node(Clauses, Errors),
	Ast2 = {clauses, Ast},
	{Ast2, Errors2};
transform_node(Tuple, Errors) when is_tuple(Tuple) ->
	[Tag, Line | Rest] = tuple_to_list(Tuple),
	{NewRest, Errors2} = lists:mapfoldl(fun(N, E) -> transform_node(N, E) end,
										Errors, Rest),
	{list_to_tuple([Tag, Line | NewRest]), Errors2};
transform_node(List, Errors) when is_list(List) ->
	lists:mapfoldl(fun(N, E) -> transform_node(N, E) end,
				   Errors, List);
transform_node(Node, Errors) ->
	{Node, Errors}.

%% Parse string state.

external_si("#{"++Rest, Acc, Out) ->
	case internal_si_code(Rest, []) of
		{ok, Rest2} ->
			external_si(Rest2, Acc, Out);
		{ok, Forms, Rest2} ->
			Out2 = case Acc of
					   [] -> Out;
					   _ -> [?string(lists:reverse(Acc)) | Out]
				   end,
			external_si(Rest2, [], [Forms | Out2]);
		{error, _} = Err ->
			Err
	end;
external_si([Char|Rest], Acc, Out) ->
	external_si(Rest, [Char|Acc], Out);
external_si([], [], Out) ->
	{ok, lists:reverse(Out)};
external_si([], Acc, Out) ->
	external_si([], [], [?string(lists:reverse(Acc)) | Out]).

%% Acc forms in #{_}.

internal_si_code([$'|Rest], Acc) ->
	internal_si_atom(Rest, [$' | Acc]);
internal_si_code([$"|Rest], Acc) ->
	internal_si_text(Rest, [$" | Acc]);
internal_si_code("}" ++ Rest, Acc) ->
	case Acc of
		[] -> {ok, Rest};
		_ ->
			case parse_string(lists:reverse(Acc)) of
				{ok, Forms} ->
					{ok, Forms, Rest};
				{error, _} = Err ->
					Err
			end
	end;
internal_si_code([Char|Rest], Acc) ->
	internal_si_code(Rest, [Char|Acc]);
internal_si_code([], _Acc) ->
	{error, "uncloused #{ "}.

internal_si_text([$"|Rest], [$\\ | _] = Acc) ->
	internal_si_text(Rest, [$" | Acc]);
internal_si_text([$"|Rest], Acc) ->
	internal_si_code(Rest, [$" | Acc]);
internal_si_text([Ch|Rest], Acc) ->
	internal_si_text(Rest, [Ch | Acc]);
internal_si_text([], _Acc) ->
	{error, "uncloused \" "}.

internal_si_atom([$'|Rest], [$\\ | _] = Acc) ->
	internal_si_atom(Rest, [$' | Acc]);
internal_si_atom([$'|Rest], Acc) ->
	internal_si_code(Rest, [$' | Acc]);
internal_si_atom([Ch|Rest], Acc) ->
	internal_si_atom(Rest, [Ch | Acc]);
internal_si_atom([], _Acc) ->
	{error, "uncloused \' "}.

si(Str) ->
	case external_si(Str, [], []) of
		{ok, Ast} ->
			{ok, erl_syntax:revert(?list(Ast))};
		{error, _} = Err ->
			Err
	end.

parse_string([]) ->
	{ok, ?list([])};
parse_string(Str) ->
	Str2 = "f() ->"++Str++".",
	{ok, Scan, _} = erl_scan:string(Str2),
	case erl_parse:parse(Scan) of
		{ok, {function,_,_,_,[{clause,_,_,_,Forms}]}} ->
			{ok, ?list(Forms)};
		{error, {_, erl_parse, [_, "'.'"]}} ->
			{error, "interpolation syntax error"};
		{error, {_, erl_parse, Reason}} ->
			{error, lists:flatten(Reason)}
	end.

pretty_print(Forms0) ->
	Forms = epp:restore_typed_record_fields([erl_syntax:revert(T) || T <- lists:flatten(Forms0)]),
	[io_lib:fwrite("~s~n",
				   [lists:flatten([erl_pp:form(Fm) ||
									  Fm <- Forms])])].

global_error_ast(Line, Reason) ->
	{error, {Line, erl_parse, Reason}}.
