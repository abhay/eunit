%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Mickaël Rémond.''
%%
%%     $Id: eunit.erl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% @copyright 2004-2005 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@csd.uu.se>
%%   [http://www.csd.uu.se/~richardc/]
%% @version {@vsn}, {@date} {@time}
%% @end

-module(eunit).

%%-define(NOTEST, true).

%% New EUnit entry point.
-export([run/1]).

%% Old EUnit entry point.
-export([run/2]).

%% Aaegis support
-export([erlfilename/1]).


-import(lists, [foldr/3]).

-include("eunit.hrl").


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


%% =====================================================================
%% New EUnit under construction


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
%% Client code (traverse, execute, print)

-record(state, {succeed = 0, fail = 0, aborted = false, browse = false}).

run(M) ->
    run_0(M, false).

list(M) ->
    run_0(M, true).

run_0(M, Browse) ->
    St1 = run_1(M, 0, #state{browse = Browse}),
    case St1#state.browse of
	true ->
	    ok;
	false ->
	    io:fwrite("================================\n"
		      "  Failed: ~w.  Succeeded: ~w.\n",
		      [St1#state.fail, St1#state.succeed]),
	    if St1#state.aborted ->
		    io:fwrite("\n*** the testing was "
			      "prematurely aborted *** \n");
	       true ->
		    ok
	    end
    end,
    if (St1#state.fail > 0) or St1#state.aborted -> failed;
       true -> ok
    end.
    
%% internal use only

run_1(M, In, St) ->
    loop(init(M), In, St).

loop(_I, _In, St = #state{aborted = true}) ->
    St;
loop(I, In, St) ->
    try next(I) of
	{T, I1} ->
	    St1 = case T of
		      #test{} ->
			  case handle_test(T, In, St#state.browse) of
			      ok -> St#state{succeed = St#state.succeed + 1};
			      error -> St#state{fail = St#state.fail + 1}
			  end;
		      #group{} ->
			  indent(In),
			  io:fwrite(T#group.label),
			  io:nl(),
			  run_1(T#group.tests, In + 1, St);
		      #context{} ->
			  try enter(St#state.browse,
				    T, fun (T) -> run_1(T, In, St) end)
			  catch
			      setup_failed ->
				  abort("context setup failed",
					"", [], St);
			      cleanup_failed ->
				  abort("context cleanup failed", "",
					[], St);
			      instantiation_failed ->
				  abort("instantiation of subtests failed",
					"", [], St)
			  end
		  end,
	    loop(I1, In, St1);
	none ->
	    St
    catch
	{bad_test, Bad} ->
	    abort("bad test descriptor", "~p", [Bad], St);
	{module_not_found, M} ->
	    abort("test module not found", "~p", [M], St);
	{generator_failed, {M, F, A}, Exception} ->
	    abort(io_lib:format("test generator failed: ~w:~w/~w",
				[M, F, A]),
		  "~p", [Exception], St)
    end.

abort(Title, Str, Args, St) ->
    io:fwrite("\n*** ~s ***\n::~s\n\n",
	      [Title, io_lib:format(Str, Args)]),
    St#state{aborted = true}.

handle_test(T, In, Browse) ->
    indent(In),
    io:fwrite("~s:~s~s~s",
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
    case Browse of
	true ->
	    io:fwrite("\n");
	false ->
	    io:fwrite("..."),
	    try run_test(T) of
		ok ->
		    io:fwrite("ok\n"),
		    ok;
		{error, _Reason} ->
		    io:fwrite("*failed*\n::~p\n\n", [_Reason]),
		    error
	    catch
		{module_not_found, M} ->
		    test_skipped("missing module: ~w", [M]),
		    error;
		{no_such_function, {M,F,A}} ->
		    test_skipped("no such function: ~w:~w/~w",
				 [M, F, A]);
		Class:Reason ->
		    test_skipped("internal error: ~p",
				 [{Class, Reason, get_stacktrace()}])
	    end
    end.

test_skipped(Str, Args) ->
    io:fwrite("** did not run **\n::~s\n\n", [io_lib:format(Str, Args)]),
    error.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.

%% ---------------------------------------------------------------------
%% Test runner

%% @spec (Test) -> ok | {error, exception()}
%% @throws wrapperError()

run_test(#test{f = F}) ->
    run_testfun(F).

run_testfun(F) ->
    try
	F()
    of _ ->
	    ok
    catch
	{eunit_failure, Term} ->
	    %% Internally generated: re-throw Term (lose the trace)
	    throw(Term);
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

wrapper_test_() ->
    {"error handling in function wrapper",
     [?_assertException(throw, {module_not_found, eunit_nonexisting},
			run_testfun(function_wrapper(eunit_nonexisting,test))),
      ?_assertException(throw,
			{no_such_function, {eunit,nonexisting_test,0}},
			run_testfun(function_wrapper(eunit,nonexisting_test))),
      ?_test({error, {error, undef, _T}}
	     = run_testfun(function_wrapper(eunit,wrapper_test_helper)))
     ]}.

wrapper_test_helper() ->
    {ok, ?MODULE:nonexisting_function()}.

-endif.

%% @spec (Browse::bool(), Tests::#context{}, Callback::(any()) -> any())
%%       -> any()
%% @throws setup_failed | instantiation_failed | cleanup_failed

enter(false, #context{setup = S, cleanup = C, instantiate = I}, F) ->
    enter(S, C, I, F);
enter(true, #context{instantiate = I}, F) ->
    %% Browse: dummy setup/cleanup and a wrapper for the instantiator
    S = fun () -> ok end,
    C = fun (_) -> ok end,
    I1 = fun (_) ->
		try browse_fun(I) of
		    {ok, _, T} ->
			T;
		    error ->
			throw(instantiation_failed)
		catch
		    _:_ ->
			throw(instantiation_failed)
		end
	 end,
    enter(S, C, I1, F).

enter(Setup, Cleanup, Instantiate, Callback) ->
    try Setup() of
	R ->
	    try Instantiate(R) of
		T ->
		    try Callback(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try Cleanup(R)
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

%% @throws {bad_test, term()}
%%       | {generator_failed, mfa(), exception()}
%%       | {module_not_found, moduleName()}

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

%% @throws {bad_test, term()}
%%       | {generator_failed, mfa(), exception()}
%%       | {module_not_found, moduleName()}

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
    #test{f = function_wrapper(M, F), module = M, name = F};
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
    case erlang:fun_info(F, type) of
	{type, external} ->
	    N;
	{type, local} ->
	    S = atom_to_list(N),
	    list_to_atom(string:sub_string(S, 2, string:chr(S, $/) - 1))
    end.

-ifndef(NOTEST).

fun_parent_test() ->
    fun_parent_test = fun_parent(fun () -> ok end).

-endif.


%% Extracting test funs from a module

%% @throws {generator_failed, mfa(), exception()}
%%       | {module_not_found, moduleName()}

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
	    throw({generator_failed, Where,
		   {Class, Reason, get_stacktrace([Where])}})
    end.


%% Wrapper for simple "named function" tests ({M,F}), which provides
%% better error reporting when the function is missing at test time.
%%
%% Note that the wrapper fun is usually called by run_testfun/1, and the
%% special exceptions thrown here are expected to be handled there.
%%
%% @throws {eunit_failure, wrapperError()}
%%
%%     wrapperError() = {no_such_function, mfa()}
%%                    | {module_not_found, moduleName()}

function_wrapper(M, F) ->
    fun () ->
 	    try M:F()
 	    catch
 		error:undef ->
 		    %% Check if it was M:F/0 that was undefined
 		    case erlang:module_loaded(M) of
 			false ->
 			    fail({module_not_found, M});
 			true ->
 			    case erlang:function_exported(M, F, 0) of
 				false ->
 				    fail({no_such_function, {M, F, 0}});
 				true ->
 				    rethrow(error, undef, [{M, F, 0}])
 			    end
 		    end
 	    end
    end.

rethrow(Class, Reason, Trace) ->
    erlang:raise(Class, Reason, get_stacktrace(Trace)).

fail(Term) ->
    throw({eunit_failure, Term}).				   

%% Getting a cleaned up stack trace. (We don't want it to include
%% eunit's own internal functions. This complicates self-testing
%% somewhat, but you can't have everything.)

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
		{error, {Class, Reason, Trace}} ->
		    erlang:raise(Class, Reason, Trace)
	    end;
	[] ->
	    error
    end.

%% Apply argument to function and report whether it succeeded (and with
%% what return value), or failed due to a simple top-level
%% function_clause error, or if it crashed in some other way.

%% @spec (F::(any()) -> any(), V::any()) -> 
%%     {ok, Result::any()} | {error, function_clause | exception()}

try_apply(F, Arg) ->
    case erlang:fun_info(F, arity) of
	{arity, 1} -> ok;
	_ -> throw({badarity, {F, 1}})
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

-ifndef(NOTEST).

browse_fun_test_() ->
    {"browsing funs",
     [?_assertThrow({badarity, {_, 1}}, browse_fun(fun () -> ok end)),
      ?_assertThrow({badarity, {_, 1}}, browse_fun(fun (_,_) -> ok end)),
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


%% =====================================================================
%% Old EUnit code

%% Run all functions in the given list of modules that match the given pattern
run(Modules, Pattern) ->
    Result = run(Modules, Pattern, {0,0,[],[]}),
    print_results(Result),
    Result.

run([], _Pattern, State) ->
    ?log("Test finished~n", []),
    State;
run([Module|Modules], Pattern, State) ->
    ?log("Running unit tests in module [~p]", [Module]),
    Exports = Module:module_info(exports),
    Result = lists:foldr(fun(Export, CurrentState = {Passed, Failed, PassedList, FailedList}) ->
				 case run_module_tests(Module, Export, Pattern) of
				     ignore -> CurrentState;
				     passed -> {Passed+1, Failed, [{Module,Export}|PassedList], FailedList};
				     failed -> {Passed, Failed+1, PassedList, [{Module,Export}|FailedList]}
				 end
			 end,
			 State,
			 Exports),
    run(Modules, Pattern, Result).

%% Only run matching functions with 0 arity
%% Returns ignore, passed, failed
%% If pattern is all, run 
run_module_tests(Module, {Function,0}, all) ->
    run_test(Module, Function);
%% otherwise select function based on pattern {suffix, String} or {prefix, String}
run_module_tests(Module, {Function,0}, _Pattern = {Pos, StringPattern}) ->
    StringExport  = atom_to_list(Function),
    Suffix = length(StringExport) - length(StringPattern) + 1,
    case {Pos, string:rstr(StringExport, StringPattern)} of
	{_,0}            -> ignore; %% Function name does not match
	{suffix, Suffix} -> run_test(Module, Function);
	{prefix, 1}      -> run_test(Module, Function)
    end;
run_module_tests(_Module, _Export, _Pattern) ->
    ignore.

%% Return passed, failed
run_test(Module, Function) ->
    TestDesc = "Running test " ++ atom_to_list(Module) ++ ":" ++ atom_to_list(Function) ++ "/0",
    case catch (Module:Function()) of
	{'EXIT', _} ->
	    ?log(TestDesc ++ " - failed.", []),
	    failed;
	_Other ->
	    ?log(TestDesc ++ " - passed.", []),
	    passed
    end.

print_results({Passed, Failed, _PassedList, _FailedList}) ->
    ?log("Summary: ~p passed, ~p failed", [Passed, Failed]).


%% ---------------------------------------------------------------------
%% This function is used by aegis
%% 
%% It compiles the given Erlang module name
%% It run all the exported functions of arity 0 in it.
%% TODO: Make this more modular
erlfilename(Erlfile) ->
    Dir = filename:dirname(Erlfile),
    Module = filename:basename(Erlfile,".erl"),
    File = filename:join(Dir, Module),
    case compile:file(Erlfile, [return_errors, binary]) of
	{error, _ErrorList, _WarningList} ->
	    ?log("Cannot compile test module: ~s~n", [Erlfile]),
	    timer:sleep(10),
	    halt(1);
	{ok, ModuleName, Binary} ->
	    {module, ModuleName} = code:load_binary(ModuleName, File, Binary),
	    Exports = ModuleName:module_info(exports),
	    ?log("Running unit tests in module [~p]", [ModuleName]),
	    Result = lists:foldl(fun({module_info,0},Acc) ->
					 Acc;
				    ({Function, 0}, Acc) ->
					 case run_test(ModuleName, Function) of
					     passed -> Acc;
					     failed -> Acc + 1
					 end;
				    (_OtherArity, Acc) ->
					 Acc
				 end,
				 0,
				 Exports),
	    case Result of
		%% No test failed
		0 -> timer:sleep(10), halt();
		%% Some tests failed
		_Other ->
		    ?log("Failure.~n", []),
		    timer:sleep(10), halt(1)
	    end
    end.
	   
