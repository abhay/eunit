%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Initial Developer of the Original Code is Richard Carlsson.''
%%
%% $Id: eunit_lib.erl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% @copyright 2004-2006 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@it.uu.se>
%%   [http://user.it.uu.se/~richardc/]
%% @private
%% @see eunit
%% @doc Utility functions for eunit

-module(eunit_lib).

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([dlist_next/1, uniq/1, fun_parent/1, is_string/1,
	 browse_fun/1, command/1, command/2, command/3,
	 trie_new/0, trie_store/2, trie_match/2]).

%% EUnit self-testing 
-include("eunit_test.hrl").


%% Type definitions for describing exceptions
%%
%% @type exception() = {exceptionClass(), Reason::term(), stackTrace()}
%%
%% @type exceptionClass() = error | exit | throw
%%
%% @type stackTrace() = [{moduleName(), functionName(),
%%			  arity() | argList()}]
%%
%% @type moduleName() = atom()
%% @type functionName() = atom()
%% @type arity() = integer()
%% @type mfa() = {moduleName(), functionName(), arity()}
%% @type argList() = [term()]


%% ---------------------------------------------------------------------
%% Deep list iterator; accepts improper lists/sublists, and also accepts
%% non-lists on the top level. The result is always presented as a list
%% (which may be improper), which is either empty or otherwise has a
%% non-list head element.

dlist_next([X | Xs]) when is_list(X) ->
    dlist_next(X, Xs);
dlist_next([_|_] = Xs) ->
    Xs;
dlist_next([]) ->
    [];
dlist_next(X) ->
    [X].

%% the first two clauses avoid pushing empty lists on the stack
dlist_next([X], Ys) when is_list(X) ->
    dlist_next(X, Ys);
dlist_next([X], Ys) ->
    [X | Ys];
dlist_next([X | Xs], Ys) when is_list(X) ->
    dlist_next(X, [Xs | Ys]);
dlist_next([X | Xs], Ys) ->
    [X | [Xs | Ys]];
dlist_next([], Xs) ->
    dlist_next(Xs).


-ifdef(TEST).
dlist_test_() ->
    {"deep list traversal",
     [?_test_("non-list term -> singleton list",
 	      [any] = dlist_next(any)),
      ?_test_("empty list -> empty list",
 	      [] = dlist_next([])),
      ?_test_("singleton list -> singleton list",
 	      [any] = dlist_next([any])),
      ?_test_("taking the head of a flat list",
 	      [1,2,3] = dlist_next([1,2,3])),
      ?_test_("skipping an initial empty list",
 	      [1,2,3] = dlist_next([[],1,2,3])),
      ?_test_("skipping nested initial empty lists",
 	      [1,2,3] = dlist_next([[[[]]],1,2,3])),
      ?_test_("skipping a final empty list",
 	      [] = dlist_next([[]])),
      ?_test_("skipping nested final empty lists",
 	      [] = dlist_next([[[[]]]])),
      ?_test_("the first element is in a sublist",
 	      [1,2,3] = dlist_next([[1],2,3])),
      ?_test_("traversing an empty list",
 	      [] = dlist_flatten([])),
      ?_test_("traversing a flat list",
 	      [1,2,3] = dlist_flatten([1,2,3])),
      ?_test_("traversing a deep list",
 	      [1,2,3] = dlist_flatten([[],[1,[2,[]],3],[]])),
      ?_test_("traversing a deep but empty list",
 	      [] = dlist_flatten([[],[[[]]],[]]))
     ]}.

%% test support
dlist_flatten(Xs) ->
    case dlist_next(Xs) of
	[X | Xs1] -> [X | dlist_flatten(Xs1)];
	[] -> []
    end.
-endif.


%% ---------------------------------------------------------------------
%% Check for proper Unicode-stringness.

is_string([C | Cs]) when is_integer(C), C >= 0, C =< 16#10ffff ->
    is_string(Cs);
is_string([_ | _]) ->
    false;
is_string([]) ->
    true;
is_string(_) ->
    false.

-ifdef(TEST).
is_string_test_() ->
    {"is_string",
     [?_assert_("no non-lists",
 		not is_string($A)),
      ?_assert_("no non-integer lists",
 		not is_string([true])),
      ?_assert_("empty string",
 		is_string("")),
      ?_assert_("ascii string",
 		is_string(lists:seq(0, 127))),
      ?_assert_("latin-1 string",
 		is_string(lists:seq(0, 255))),
      ?_assert_("unicode string",
 		is_string([0, $A, 16#10fffe, 16#10ffff])),
      ?_assert_("not above unicode range",
 		not is_string([0, $A, 16#110000])),
      ?_assert_("no negative codepoints",
 		not is_string([$A, -1, 0]))
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Get the name of the containing function for a fun. (This is encoded
%% in the name of the generated function that implements the fun.)

fun_parent(F) ->
    {name, N} = erlang:fun_info(F, name),
    case erlang:fun_info(F, type) of
	{type, external} ->
	    N;
	{type, local} ->
	    S = atom_to_list(N),
	    list_to_atom(string:sub_string(S, 2, string:chr(S, $/) - 1))
    end.

-ifdef(TEST).
fun_parent_test() ->
    fun_parent_test = fun_parent(fun () -> ok end).
-endif.


%% ---------------------------------------------------------------------
%% Ye olde uniq function

uniq([X, X | Xs]) -> uniq([X | Xs]);    
uniq([X | Xs]) -> [X | uniq(Xs)];
uniq([]) -> [].

-ifdef(TEST).
uniq_test_() ->
    {"uniq",
     [?_assertError(function_clause, uniq(ok)),
      ?_assertError(function_clause, uniq([1|2])),
      ?_test([] = uniq([])),
      ?_test([1,2,3] = uniq([1,2,3])),
      ?_test([1,2,3] = uniq([1,2,2,3])),
      ?_test([1,2,3,2,1] = uniq([1,2,2,3,2,2,1])),
      ?_test([1,2,3] = uniq([1,1,1,2,2,2,3,3,3])),
      ?_test(["1","2","3"] = uniq(["1","1","2","2","3","3"]))
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Apply arbitrary unary function F with dummy arguments "until it
%% works". (F must be side effect free! It will be called repeatedly.)
%% No exceptions will be thrown unless the function actually crashes for
%% some other reason than being unable to match the argument.

%% @spec (F::(any()) -> any()) -> {ok, Value::any(), Result::any()} | error

browse_fun(F) ->
    browse_fun(F, arg_values()).

browse_fun(F, Next) ->
    case Next() of
	[V | Next1] ->
	    case try_apply(F, V) of
		{ok, Result} ->
		    {ok, V, Result};
		{error, function_clause} ->
		    browse_fun(F, Next1);
		{error, badarity} ->
		    erlang:error({badarity, {F, 1}});
		{error, {Class, Reason, Trace}} ->
		    erlang:raise(Class, Reason, Trace)
	    end;
	[] ->
	    error
    end.

%% Apply argument to function and report whether it succeeded (and with
%% what return value), or failed due to bad arity or a simple top-level
%% function_clause error, or if it crashed in some other way.

%% @spec (F::(any()) -> any(), V::any()) -> 
%%     {ok, Result::any()}
%%   | {error, function_clause | badarity | eunit_test:exception()}

try_apply(F, Arg) ->
    case erlang:fun_info(F, arity) of
	{arity, 1} ->
	    {module, M} = erlang:fun_info(F, module),
	    {name, N} = erlang:fun_info(F, name),
	    try_apply(F, Arg, M, N);
	_ ->
	    {error, badarity}
    end.

try_apply(F, Arg, M, N) ->
    try F(Arg) of
	X -> {ok, X}
    catch
	error:function_clause ->
	    case erlang:get_stacktrace() of
		[{M, N, _Args} | _] ->
		    {error, function_clause};
		Trace ->
		    {error, {error, function_clause, Trace}}
	    end;
	  Class:Reason ->
	    {error, {Class, Reason, erlang:get_stacktrace()}}
    end.

%% test value producers for function browsing

arg_values() ->
    Vs = [undefined, ok, true, false, 0, 1],
    fun () -> arg_values(Vs) end.

arg_values([V | Vs]) ->
    [V | fun () -> arg_values(Vs) end];
arg_values(_) ->
    (arg_tuples())().

arg_tuples() ->
    fun () -> arg_tuples(0) end.

arg_tuples(N) when N >= 0, N =< 12 ->
    [erlang:make_tuple(N, undefined) | fun () -> arg_tuples(N + 1) end];
arg_tuples(_) ->
    (arg_lists())().

arg_lists() ->
    fun () -> arg_lists(0) end.

arg_lists(N) when N >= 0, N =< 12 ->
    [lists:duplicate(N, undefined) | fun () -> arg_lists(N + 1) end];
arg_lists(_) ->
    [].

-ifdef(TEST).
browse_fun_test_() ->
    {"browsing funs",
     [?_assertError({badarity, {_, 1}}, browse_fun(fun () -> ok end)),
      ?_assertError({badarity, {_, 1}}, browse_fun(fun (_,_) -> ok end)),
      ?_test({ok, _, 17} = browse_fun(fun (_) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun (undefined) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun (ok) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun (true) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ({}) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ({_}) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ({_,_}) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ({_,_,_}) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ([]) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ([_]) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ([_,_]) -> 17 end)),
      ?_test({ok, _, 17} = browse_fun(fun ([_,_,_]) -> 17 end))
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Replacement for os:cmd

command(Cmd) ->
    command(Cmd, "").

command(Cmd, Dir) ->
    command(Cmd, Dir, []).

command(Cmd, Dir, Env) ->
    CD = if Dir == "" -> [];
	    true -> [{cd, Dir}]
	 end,
    SetEnv = if Env == [] -> []; 
		true -> [{env, Env}]
	     end,
    Opt = CD ++ SetEnv ++ [stream, exit_status, use_stdio,
			   stderr_to_stdout, in, eof],
    P = open_port({spawn, Cmd}, Opt),
    get_data(P, []).

get_data(P, D) ->
    receive
	{P, {data, D1}} ->
	    get_data(P, [D|D1]);
	{P, eof} ->
	    port_close(P),    
	    receive
		{P, {exit_status, N}} ->
		    {N, lists:flatten(D)}
	    end
    end.

-ifdef(TEST).

cmd_test_() ->
    [{"command execution, status, and output",
      [?_test({0, "hello\n"} = ?_cmd_("echo hello")),
       ?_cmd("echo hello"),
       ?_assertCmdStatus(0, "true"),
       ?_assertCmdStatus(1, "false"),
       ?_assertCmd("true"),
       ?_assertCmdOutput("hello\n", "echo hello"),
       ?_assertCmdOutput("hello", "echo -n hello")
      ]},
     {"file setup and cleanup",
      setup,
      fun () -> ?cmd("mktemp") end,
      fun (File) -> ?cmd("rm " ++ File) end,
      fun (File) ->
	      [?_assertCmd("echo xyzzy >" ++ File),
	       ?_assertCmdOutput("xyzzy\n", "cat " ++ File)]
      end}
    ].

-endif. % TEST


%% ---------------------------------------------------------------------
%% A trie for remembering and checking least specific cancelled events
%% (an empty list `[]' simply represents a stored empty list, i.e., all
%% events will match, while an empty tree means that no events match).

trie_new() ->
    gb_trees:empty().

trie_store([_ | _], []) ->
    [];
trie_store([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    if Es == [] ->
		    %% overwrite any previous more specific pattern
		    gb_trees:insert(E, [], T);
	       true ->
		    gb_trees:insert(E, trie_store(Es, gb_trees:empty()),
				    T)
	    end;
	{value, []} ->
	    T;  %% prefix already stored
	{value, T1} ->
	    gb_trees:update(E, trie_store(Es, T1), T)
    end;
trie_store([], _T) ->
    [].

trie_match([_ | _], []) ->
    prefix;
trie_match([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    no;
	{value, []} ->
	    if Es == [] -> exact;
	       true -> prefix
	    end;
	{value, T1} ->
	    trie_match(Es, T1)
    end;
trie_match([], []) ->
    exact;
trie_match([], _T) ->
    no.

-ifdef(TEST).

trie_test_() ->
    [{"basic representation",
      [?_assert(trie_new() == gb_trees:empty()),
       ?_assert(trie_store([1], trie_new())
		== gb_trees:insert(1, [], gb_trees:empty())),
       ?_assert(trie_store([1,2], trie_new())
		== gb_trees:insert(1,
				   gb_trees:insert(2, [],
						   gb_trees:empty()),
				   gb_trees:empty())),
       ?_assert([] == trie_store([1], [])),
       ?_assert([] == trie_store([], gb_trees:empty()))
      ]},
     {"basic storing and matching",
      [?_test(no = trie_match([], trie_new())),
       ?_test(exact = trie_match([], trie_store([], trie_new()))),
       ?_test(no = trie_match([], trie_store([1], trie_new()))),
       ?_test(exact = trie_match([1], trie_store([1], trie_new()))),
       ?_test(prefix = trie_match([1,2], trie_store([1], trie_new()))),
       ?_test(no = trie_match([1], trie_store([1,2], trie_new()))),
       ?_test(no = trie_match([1,3], trie_store([1,2], trie_new()))),
       ?_test(exact = trie_match([1,2,3,4,5],
				 trie_store([1,2,3,4,5], trie_new()))),
       ?_test(prefix = trie_match([1,2,3,4,5],
				  trie_store([1,2,3], trie_new()))),
       ?_test(no = trie_match([1,2,2,4,5],
			       trie_store([1,2,3], trie_new())))
      ]},
     {"matching with partially overlapping patterns",
      setup,
      fun () ->
	      trie_store([1,3,2], trie_store([1,2,3], trie_new()))
      end,
      fun (T) ->
	      [?_test(no = trie_match([], T)),
	       ?_test(no = trie_match([1], T)),
	       ?_test(no = trie_match([1,2], T)),
	       ?_test(no = trie_match([1,3], T)),
	       ?_test(exact = trie_match([1,2,3], T)),
	       ?_test(exact = trie_match([1,3,2], T)),
	       ?_test(no = trie_match([1,2,2], T)),
	       ?_test(no = trie_match([1,3,3], T)),
	       ?_test(prefix = trie_match([1,2,3,4], T)),
	       ?_test(prefix = trie_match([1,3,2,1], T))]
      end},
     {"matching with more general pattern overriding less general",
      setup,
      fun () -> trie_store([1], trie_store([1,2,3], trie_new())) end,
      fun (_) -> ok end,
      fun (T) ->
   	      [?_test(no = trie_match([], T)),
	       ?_test(exact = trie_match([1], T)),
 	       ?_test(prefix = trie_match([1,2], T)),
 	       ?_test(prefix = trie_match([1,2,3], T)),
 	       ?_test(prefix = trie_match([1,2,3,4], T))]
      end}
    ].

-endif.  % TEST
