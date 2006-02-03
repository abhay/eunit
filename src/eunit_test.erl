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
%% The Initial Developer of the Original Code is Richard Carlsson.''
%%
%% File: eunit_test.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test running functionality

-module(eunit_test).

-export([run_testfun/1, function_wrapper/2]).


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


-ifndef(NOTEST).
macro_test_() ->
    {"macro definitions",
     [{?LINE, fun () ->
 		      {?LINE, F} = ?_test(undefined),
 		      {ok, ok} = run_testfun(F)
 	      end},
      ?_test(begin
 		 {?LINE, F} = ?_assert(true),
 		 {ok, ok} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assert(false),
 		 {error,{throw,assertion_failed,_}} = run_testfun(F)
 	     end),
      ?_test(begin
 		 {?LINE, F} = ?_assertException(error, badarith,
 						erlang:error(badarith)),
 		 {ok, ok} = run_testfun(F)
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


-ifndef(NOTEST).
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
%% Getting a cleaned up stack trace. (We don't want it to include
%% eunit's own internal functions. This complicates self-testing
%% somewhat, but you can't have everything.)

get_stacktrace() ->
    get_stacktrace([]).

get_stacktrace(Ts) ->
    eunit_lib:uniq(prune_trace(erlang:get_stacktrace(), Ts)).

prune_trace([{eunit_test, run_testfun, 1} | _Rest], Tail) ->
    Tail;
prune_trace([T | Ts], Tail) ->
    [T | prune_trace(Ts, Tail)];
prune_trace([], Tail) ->
    Tail.