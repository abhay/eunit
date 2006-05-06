%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test running functionality

-module(eunit_test).

-export([run_testfun/1, function_wrapper/2, enter_context/4,
	 browse_context/2]).


-include("eunit.hrl").
-include("eunit_internal.hrl").


%% EUnit self-testing 
-include("eunit_test.hrl").


%% ---------------------------------------------------------------------
%% Test runner

%% @spec ((any()) -> any()) -> {ok, Value} | {error, eunit_lib:exception()}
%% @throws wrapperError()

%% Note: if this function is moved, renamed, or gets a different number
%% of arguments, the function eunit_test:prune_trace/2 must be updated
%% correspondingly.

run_testfun(F) ->
    try
	F()
    of Value ->
	    {ok, Value}
    catch
	{eunit_failure, Term} ->
	    %% Internally generated: re-throw Term (lose the trace)
	    throw(Term);
	Class:Reason ->
	    {error, {Class, Reason, get_stacktrace()}}
    end.


-ifdef(TEST).
macro_test_() ->
    {"macro definitions",
     [{?LINE, fun () ->
 		      {?LINE, F} = ?_test(undefined),
 		      {ok, undefined} = run_testfun(F)
 	      end},
      ?_test(begin
 		 {?LINE, F} = ?_assert(true),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assert(false),
 		 {error,{error,{assertion_failed,_,false},_}} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assert([]),
 		 {error,{error,{assertion_failed,_,{not_a_boolean,[]}},_}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertNot(false),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertNot(true),
 		 {error,{error,{assertion_failed,_,false},_}} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarith,
 						erlang:error(badarith)),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarith, ok),
 		 {error,{error,{assertException_failed,_,
				{expected,_},{unexpected_success,ok}},_}}
		     = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarg,
 						erlang:error(badarith)),
 		 {error,{error,{assertException_failed,_,
				{expected,_},{unexpected_exception,
					      {error,badarith,_}}},_}}
		     = run_testfun(F)
 	     end)
     ]}.
-endif.


%% ---------------------------------------------------------------------
%% Wrapper for simple "named function" tests ({M,F}), which provides
%% better error reporting when the function is missing at test time.
%%
%% Note that the wrapper fun is usually called by run_testfun/1, and the
%% special exceptions thrown here are expected to be handled there.
%%
%% @throws {eunit_failure, wrapperError()}
%%
%% @type wrapperError() = {no_such_function, mfa()}
%%                      | {module_not_found, moduleName()}

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
 				    fail({no_such_function, {M,F,0}});
 				true ->
 				    rethrow(error, undef, [{M,F,0}])
 			    end
 		    end
 	    end
    end.

rethrow(Class, Reason, Trace) ->
    erlang:raise(Class, Reason, get_stacktrace(Trace)).

fail(Term) ->
    throw({eunit_failure, Term}).				   


-ifdef(TEST).
wrapper_test_() ->
    {"error handling in function wrapper",
     [?_assertException(throw, {module_not_found, eunit_nonexisting},
  			run_testfun(function_wrapper(eunit_nonexisting,test))),
      ?_assertException(throw,
  			{no_such_function, {?MODULE,nonexisting_test,0}},
  			run_testfun(function_wrapper(?MODULE,nonexisting_test))),
      ?_test({error, {error, undef, _T}}
  	     = run_testfun(function_wrapper(?MODULE,wrapper_test_exported_)))
     ]}.

%% this must be exported (done automatically by the autoexport transform)
wrapper_test_exported_() ->
    {ok, ?MODULE:nonexisting_function()}.
-endif.


%% ---------------------------------------------------------------------
%% Entering a setup-context, with guaranteed cleanup.

%% @spec (Setup, Cleanup, Instantiate, Callback) -> any()
%%    Setup = () -> any()
%%    Cleanup = (any()) -> any()
%%    Instantiate = (any()) -> tests()
%%    Callback = (tests()) -> any()
%% @throws {ErrorType, eunit_lib:exception()}
%% ErrorType = setup_failed | instantiation_failed | cleanup_failed

%% Note: if this function is moved, renamed, or gets a different number
%% of arguments, the function eunit_test:prune_trace/2 must be updated
%% correspondingly.

enter_context(Setup, Cleanup, Instantiate, Callback) ->
    try Setup() of
	R ->
	    try Instantiate(R) of
		T ->
		    try Callback(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try Cleanup(R)
			catch
			    Class:Term ->
				context_failure(cleanup_failed,
						Class, Term)
			end
		    end
	    catch
		Class:Term ->
		    context_failure(instantiation_failed, Class, Term)
	    end
    catch
	Class:Term ->
	    context_failure(setup_failed, Class, Term)
    end.

context_failure(Type, Class, Term) ->
    throw({Type, {Class, Term, get_stacktrace()}}).

%% Instantiates a context with dummy values to make browsing possible
%% @throws {instantiation_failed, eunit_lib:exception()}

%% Note: if this function is moved, renamed, or gets a different number
%% of arguments, the function eunit_test:prune_trace/2 must be updated
%% correspondingly.

browse_context(I, F) ->
    %% Browse: dummy setup/cleanup and a wrapper for the instantiator
    S = fun () -> ok end,
    C = fun (_) -> ok end,
    I1 = fun (_) ->
		try eunit_lib:browse_fun(I) of
		    {_, T} -> T
		catch
		    Class:Term ->
			context_failure(instantiation_failed, Class,
					Term)
		end
	 end,
    enter_context(S, C, I1, F).


%% ---------------------------------------------------------------------
%% Getting a cleaned up stack trace. (We don't want it to include
%% eunit's own internal functions. This complicates self-testing
%% somewhat, but you can't have everything.)

get_stacktrace() ->
    get_stacktrace([]).

get_stacktrace(Ts) ->
    eunit_lib:uniq(prune_trace(erlang:get_stacktrace(), Ts)).

prune_trace([{eunit_test, run_testfun, 1} | _Rest], Tail) ->
    Tail;
prune_trace([{eunit_test, enter_context, 4} | _Rest], Tail) ->
    Tail;
prune_trace([{eunit_test, browse_context, 2} | _Rest], Tail) ->
    Tail;
prune_trace([T | Ts], Tail) ->
    [T | prune_trace(Ts, Tail)];
prune_trace([], Tail) ->
    Tail.
