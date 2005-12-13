%% -------------------------------------------------------------------
%% File: unit.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richard@virtutech.com>
%% @copyright 2005 Richard Carlsson
%% @doc 

-module(unit).

-export([run/0]).

-import(lists, [foldr/3]).

-include("unit.hrl").

%%-define(NOTEST, true).

%% ---------------------------------------------------------------------
%% Self-testing interface

-ifndef(NOTEST).
-compile(export_all).
-export([selftest/0,
	 macro_test_/0,
	 dlist_test_/0,
	 is_string_test_/0,
	 fun_parent_test/0,
	 uniq_test_/0,
	 browse_fun_test_/0 
	]).
selftest() -> run(?MODULE).
-endif.

%% ---------------------------------------------------------------------
%% Protocol records

-record(test, {f = undefined,
	       desc = undefined,
	       module = undefined,
	       name = undefined,
	       line = 0
	      }).

-record(context, {setup = undefined,
		  cleanup = undefined,
		  instantiate = undefined}).

-record(group, {label = "",
		order = undefined,
		tests = undefined}).


%% ---------------------------------------------------------------------
%% Client code (traverse, execute, print)

-record(state, {succeed = 0, fail = 0}).

%% interface
run() ->
    run(test).

run(M) ->
    St1 = run(M, 0, #state{}),
    io:fwrite("================================\n"
	      "  Failed: ~w.  Succeeded: ~w.\n",
	      [St1#state.fail, St1#state.succeed]),
    if St1#state.fail > 0 -> failed;
       true -> ok
    end.
    
%% internal use only
run(M, In, St) ->
    loop(init(M), In, St).
    
loop(I, In, St) ->
    case next(I) of
	{T, I1} ->
	    St1 = case T of
		      #test{} ->
			  case run(T, In) of
			      ok -> St#state{succeed = St#state.succeed + 1};
			      error -> St#state{fail = St#state.fail + 1}
			  end;
		      #group{} ->
			  indent(In),
			  io:fwrite(T#group.label),
			  io:nl(),
			  run(T#group.tests, In + 1, St);
		      #context{} ->
			  enter(T, fun (T) -> run(T, In, St) end)
		  end,
	    loop(I1, In, St1);
	none ->
	    St
    end.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.
    
run(T, In) ->
    indent(In),
    io:fwrite("~s:~s~s~s...",
	      [T#test.module, 
	       case T#test.line of
		   0 -> "";
		   L -> io_lib:fwrite("~w:", [L])
	       end,
	       T#test.name,
	       case T#test.desc of
		   undefined -> "";
		   Desc -> io_lib:fwrite(" (~s)", [Desc])
	       end]),
    case run_test(T) of
	ok ->
	    io:fwrite("ok\n"),
	    ok;
	{error, _Reason} ->
	    io:fwrite("*failed*\n::~p\n\n", [_Reason]),
	    error
    end.

%% ---------------------------------------------------------------------
%% Test runner

run_test(#test{f = F}) ->
    run_testfun(F).

run_testfun(F) ->
    try
	F()
    of _ ->
	    ok
    catch
	throw:{unit_test_internal_error, Term} ->
	    %% Internally generated - pass it on (lose the trace)
	    throw(Term);
	throw:{unit_test_error, Reason} ->
	    %% Compensate for any internal re-throwing
	    {error, Reason};
	Class:Reason ->
	    {error, {Class, Reason, get_stacktrace()}}
    end.

-ifndef(NOTEST).

macro_test_() ->
    {"macro definitions",
     [{?LINE, fun () ->
		      {?LINE, F} = ?_test(undefined),
		      ok = run_testfun(F)
	      end},
      ?_test(begin
		 {?LINE, F} = ?_assert(true),
		 ok = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assert(false),
		 {error,{throw,assertion_failed,_}} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertException(error, badarith,
						erlang:error(badarith)),
		 ok = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertException(error, badarith, ok),
		 {error,{throw,{unexpected_success,ok},_}} = run_testfun(F)
	     end),
      ?_test(begin
		 {?LINE, F} = ?_assertException(error, badarg,
						erlang:error(badarith)),
		 {error,{throw,{unexpected_exception,
				{error,badarith,_}},_}} = run_testfun(F)
	     end)
     ]}.

-endif.


enter(#context{setup = S, cleanup = C, instantiate = M}, Fun) ->
    try S() of
	R ->
	    try M(R) of
		T ->
		    try Fun(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try C(R)
			catch
			    _:_ -> throw(cleanup_failed)
			end
		    end
	    catch
		_:_ ->
		    throw(instantiation_failed)
	    end
    catch
	_:_ ->
	    throw(setup_failed)
    end.


%% ---------------------------------------------------------------------
%% Abstract test set iterator

-record(iter,
	{prev = [],
	 next = [],
	 tests = []}).

init(Tests) ->
    #iter{tests = Tests}.

next(I = #iter{next = []}) ->
    case tests__next(I#iter.tests) of
	{T, Tests} ->
	    {T, I#iter{prev = [T | I#iter.prev],
		       tests = Tests}};
	none ->
	    none
    end;
next(I = #iter{next = [T | Ts]}) ->
    {T, I#iter{next = Ts,
	       prev = [T | I#iter.prev]}}.


prev(#iter{prev = []}) ->
    none;
prev(#iter{prev = [T | Ts]} = I) ->
    {T, I#iter{prev = Ts,
	       next = [T | I#iter.next]}}.


%% ---------------------------------------------------------------------
%% Concrete test representation iterator

tests__next(Tests) ->
    case dlist__next(Tests) of
	[T | Ts] ->
	    case analyze_test(T) of
		Ts1 when is_list(Ts1) ->
		    tests__next([Ts1 | Ts]);
		T1 ->
		    {T1, Ts}
	    end;
	[] ->
	    none
    end.

analyze_test({with, S, C, F}) ->
    #context{setup = S, cleanup = C, instantiate = F};
analyze_test({foreach, S, C, Fs}) when is_list(Fs) ->
    case dlist__next(Fs) of
	[F | Fs1] ->
	    [{with, S, C, F} | {foreach, S, C, Fs1}];
	[] ->
	    []
    end;
analyze_test({foreach1, S1, C1, Ps}) when is_list(Ps) ->
    case dlist__next(Ps) of
	[P | Ps1] ->
	    case P of
		{A, F1} ->
		    S = fun () -> S1(A) end,
		    C = fun (X) -> C1(A, X) end,
		    F = fun (X) -> F1(A, X) end,
		    [{with, S, C, F} | {foreach1, S1, C1, Ps1}];
		_ ->
		    throw({bad_test, P})
	    end;
	[] ->
	    []
    end;
analyze_test({S, T}) when is_list(S) ->
    case is_string(S) of
	true ->
	    case analyze_test(T) of
		T1 = #test{} ->
		    (T1)#test{desc = S};
		_ ->
		    %% redo analysis of T later
		    #group{label = S, tests = T}
	    end;
	false ->
	    throw({bad_test, {S, T}})
    end;
analyze_test(T) ->
    analyze_plain_test(T).

analyze_plain_test({L, F}) when is_integer(L), L >= 0 ->
    (analyze_plain_test(F))#test{line = L};
analyze_plain_test(F) ->
    analyze_function(F).

analyze_function(F) when is_function(F) ->
    case erlang:fun_info(F, arity) of
	{arity, 0} ->
	    {module, M} = erlang:fun_info(F, module),
	    #test{f = F, module = M, name = fun_parent(F)};
	_ ->
	    throw({bad_test, F})
    end;
analyze_function({M, F}) when is_atom(M), is_atom(F) ->
    #test{f = named_function_wrapper(M, F), module = M, name = F};
analyze_function(M) when is_atom(M) ->
    module_testfuns(M);
analyze_function(T) when is_list(T) ->
    T;
analyze_function(F) ->
    throw({bad_test, F}).



%% ---------------------------------------------------------------------
%% Deep list iterator; accepts improper lists/sublists, and also accepts
%% non-lists on the top level. The result is always presented as a list
%% (which may be improper), which is either empty or otherwise has a
%% non-list head element.

dlist__next([X | Xs]) when is_list(X) ->
    dlist__next(X, Xs);
dlist__next([_|_] = Xs) ->
    Xs;
dlist__next([]) ->
    [];
dlist__next(X) ->
    [X].

%% the first two clauses avoid pushing empty lists on the stack
dlist__next([X], Ys) when is_list(X) ->
    dlist__next(X, Ys);
dlist__next([X], Ys) ->
    [X | Ys];
dlist__next([X | Xs], Ys) when is_list(X) ->
    dlist__next(X, [Xs | Ys]);
dlist__next([X | Xs], Ys) ->
    [X | [Xs | Ys]];
dlist__next([], Xs) ->
    dlist__next(Xs).


-ifndef(NOTEST).

dlist_test_() ->
    {"deep list traversal",
     [?_test1("non-list term -> singleton list",
	      [any] = dlist__next(any)),
      ?_test1("empty list -> empty list",
	      [] = dlist__next([])),
      ?_test1("singleton list -> singleton list",
	      [any] = dlist__next([any])),
      ?_test1("taking the head of a flat list",
	      [1,2,3] = dlist__next([1,2,3])),
      ?_test1("skipping an initial empty list",
	      [1,2,3] = dlist__next([[],1,2,3])),
      ?_test1("skipping nested initial empty lists",
	      [1,2,3] = dlist__next([[[[]]],1,2,3])),
      ?_test1("skipping a final empty list",
	      [] = dlist__next([[]])),
      ?_test1("skipping nested final empty lists",
	      [] = dlist__next([[[[]]]])),
      ?_test1("the first element is in a sublist",
	      [1,2,3] = dlist__next([[1],2,3])),
      ?_test1("traversing an empty list",
	      [] = dlist__flatten([])),
      ?_test1("traversing a flat list",
	      [1,2,3] = dlist__flatten([1,2,3])),
      ?_test1("traversing a deep list",
	      [1,2,3] = dlist__flatten([[],[1,[2,[]],3],[]])),
      ?_test1("traversing a deep but empty list",
	      [] = dlist__flatten([[],[[[]]],[]]))
     ]}.

%% test support
dlist__flatten(Xs) ->
    case dlist__next(Xs) of
	[X | Xs1] -> [X | dlist__flatten(Xs1)];
	[] -> []
    end.

-endif.


%% ---------------------------------------------------------------------
%% Support functions

is_string([C | Cs]) when is_integer(C), C >= 0, C =< 16#10ffff ->
    is_string(Cs);
is_string([_ | _]) ->
    false;
is_string([]) ->
    true;
is_string(_) ->
    false.

-ifndef(NOTEST).

is_string_test_() ->
    {"is_string",
     [?_assert1("no non-lists",
		not is_string($A)),
      ?_assert1("no non-integer lists",
		not is_string([true])),
      ?_assert1("empty string",
		is_string("")),
      ?_assert1("ascii string",
		is_string(lists:seq(0, 127))),
      ?_assert1("latin-1 string",
		is_string(lists:seq(0, 255))),
      ?_assert1("unicode string",
		is_string([0, $A, 16#10fffe, 16#10ffff])),
      ?_assert1("not above unicode range",
		not is_string([0, $A, 16#110000])),
      ?_assert1("no negative codepoints",
		not is_string([$A, -1, 0]))
     ]}.

-endif.


fun_parent(F) ->
    {name, N} = erlang:fun_info(F, name),
    S = atom_to_list(N),
    list_to_atom(string:sub_string(S, 2, string:chr(S, $/) - 1)).

-ifndef(NOTEST).

fun_parent_test() ->
    fun_parent_test = fun_parent(fun () -> ok end).

-endif.


%% Extracting test funs from a module

module_testfuns(M) ->
    TestSuffix = "_test",
    GeneratorSuffix = "_test_",
    try M:module_info(exports) of
	Es ->
	    foldr(fun ({F, 0}, Fs) ->
			  N = atom_to_list(F),
			  case lists:suffix(TestSuffix, N) of
			      true ->
				  [{M,F} | Fs];
			      false ->
				  case lists:suffix(GeneratorSuffix, N) of
				      true ->
					  [generate_testfun(M, F) | Fs];
				      false ->
					  Fs
				  end
			  end;
		      (_, Fs) ->
			  Fs
		  end,
		  [],
		  Es)
    catch
	error:undef -> 
	    throw({module_not_found, M})
    end.

generate_testfun(M, F) ->
    try M:F() of
	Fun -> Fun
    catch
	Class:Reason ->
	    Where = {M, F, 0},
	    throw({{generator_failed, Where},
		   {Class, Reason, get_stacktrace([Where])}})
    end.


%% Better error reporting from named function tests

named_function_wrapper(M, F) ->
    %% This attempts to detect named functions missing at test time.
    fun () ->
 	    try M:F()
 	    catch
 		error:undef ->
 		    %% Check if it was M:F/0 that was undefined
 		    case erlang:module_loaded(M) of
 			false ->
 			    internal_error({module_not_found, M});
 			true ->
 			    case erlang:function_exported(M, F, 0) of
 				false ->
 				    internal_error({no_such_function,
						    {M, F, 0}});
 				true ->
 				    external_error(error, undef,
						   [{M, F, 0}])
 			    end
 		    end
 	    end
    end.

external_error(Class, Reason, Trace) ->
    throw({unit_test_error, {Class, Reason, get_stacktrace(Trace)}}).

internal_error(Term) ->
    throw({unit_test_internal_error, Term}).				   


%% Cleaning up a stack trace

get_stacktrace() ->
    get_stacktrace([]).

get_stacktrace(Ts) ->
    uniq(prune_trace(erlang:get_stacktrace(), Ts)).

prune_trace([{?MODULE, _, _} | _], Tail) ->
    Tail;
prune_trace([T | Ts], Tail) ->
    [T | prune_trace(Ts, Tail)];
prune_trace([], Tail) ->
    Tail.

uniq([X, X | Xs]) -> uniq([X | Xs]);    
uniq([X | Xs]) -> [X | uniq(Xs)];
uniq([]) -> [].

-ifndef(NOTEST).

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

%% Apply arbitrary unary function F with dummy arguments. (F must be
%% side effect free! It will be called repeatedly.)

browse_fun(F) ->
    browse_fun(F, values()).

browse_fun(F, Next) ->
    case Next() of
	[V | Next1] ->
	    case try_apply(F, V) of
		{ok, Result} ->
		    {ok, V, Result};
		{error, function_clause} ->
		    browse_fun(F, Next1);
		{error, Reason} ->
		    {error, Reason}
	    end;
	[] ->	
	    {error, no_match}
    end.

try_apply(F, Arg) ->
    case erlang:fun_info(F, arity) of
	{arity, 1} -> ok;
	_ -> throw(badarity)
    end,
    {module, M} = erlang:fun_info(F, module),
    {name, N} = erlang:fun_info(F, name),
    try F(Arg) of
	X -> {ok, X}
    catch
	error:function_clause ->
	    case erlang:get_stacktrace() of
		[{M, N, _} | _] ->
		    {error, function_clause};
		Trace ->
		    Trace1 = uniq(prune_trace(Trace, [{M, N, 1}])),
		    {error, {error, function_clause, Trace1}}
	    end;
	Class:Reason ->
	    {error, {Class, Reason, get_stacktrace([{M, N, 1}])}}
    end.

%% test value producers for function browsing

values() ->
    Vs = [undefined, ok, true],
    fun () -> values(Vs) end.

values([V | Vs]) ->
    [V | fun () -> values(Vs) end];
values(_) ->
    (tuples())().

tuples() ->
    fun () -> tuples(0) end.

tuples(N) when N >= 0, N =< 12 ->
    [erlang:make_tuple(N, undefined) | fun () -> tuples(N + 1) end];
tuples(_) ->
    (lists())().

lists() ->
    fun () -> lists(0) end.

lists(N) when N >= 0, N =< 12 ->
    [lists:duplicate(N, undefined) | fun () -> lists(N + 1) end];
lists(_) ->
    [].

-ifndef(NOTEST).

browse_fun_test_() ->
    {"browsing funs",
     [?_assertThrow(badarity, browse_fun(fun () -> ok end)),
      ?_assertThrow(badarity, browse_fun(fun (_,_) -> ok end)),
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
