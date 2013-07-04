String interpolation transform for Erlang
====================

String interpolation transformer inline variables to string which is argument of function ```'$'``` with ```#{Var}``` syntax.
The result of such trunsformation is ```iolist()```.
```Erlang 
'$'("text #{Var} rest text").
%% is same as
["text ", [Var], " rest text"].


'$'("text #{Var1, Var2} rest text #{Var3}").
%% is same as
["text ", [Var1, Var2], " rest text", [Var3]].
```
Also it can include not only variables, but any other valid erlang expression.

```Erlang

'$'("text #{f(Var1, '$'(\"Var2=#{Var2}\")} rest text").
%% is same as
["text ", [f(Var1, ["Var2=",[Var2]])], " rest text"].

```

Usage example:
```Erlang

-module(si).
-compile({parse_transform, string_interpolation}).

-export([example/0]).

example() ->
   A = "A",
   B = atom,
   '$'("Here is string A=#{A} and atom B=#{atom_to_list(B)}").
```
