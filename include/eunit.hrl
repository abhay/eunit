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
%% $Id: eunit.hrl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

%% Including this file turns on testing, unless explicitly disabled by
%% defining NOTEST. If both NOTEST and TEST are defined, then TEST takes
%% precedence, and NOTEST will become undefined. The macro EUNIT will be
%% defined if and only if TEST is defined, after including this file.

-ifndef(EUNIT_HRL).
-define(EUNIT_HRL, true).

%% allow defining TEST to override NOTEST
-ifdef(TEST).
-undef(NOTEST).
-endif.

%% note that the main switch used within this file is NOTEST; however,
%% both TEST and EUNIT may be used to check whether testing is enabled
-ifndef(NOTEST).
-ifndef(TEST).
-define(TEST, true).
-endif.
-ifndef(EUNIT).
-define(EUNIT, true).
-endif.
-endif.

%% The macros should be available even if testing is turned off, and
%% should preferably not require EUnit to be present at runtime.
%% 
%% We must use fun-call wrappers ((fun () -> ... end)()) to avoid
%% exporting local variables, and furthermore we only use variable names
%% prefixed with "__", that hopefully will not be bound outside the fun.

-ifdef(NOTEST).
-ifndef(assert).
-define(assert(BoolExpr),ok).
-endif.
-else.
%% The assert macro is written the way it is so as not to cause warnings
%% for clauses that cannot match, even if the expression is a constant.
-undef(assert).
-define(assert(BoolExpr),
	((fun () ->
	    case (BoolExpr) of
		true -> ok;
		__V -> erlang:error({assertion_failed, (??BoolExpr),
				     case __V of false -> __V;
					 _ -> {not_a_boolean,__V}
				     end})
	    end
	  end)())).
-endif.
-define(assertNot(BoolExpr), ?assert(not (BoolExpr))).

-define(_test(Expr), {?LINE, fun () -> (Expr) end}).

-define(_assert(BoolExpr), ?_test(?assert(BoolExpr))).

-define(_assertNot(BoolExpr), ?_assert(not (BoolExpr))).

%% This is mostly a convenience which gives more detailed reports.
%% Note: Guard is a guarded pattern, and can not be used for value.
-ifdef(NOTEST).
-define(assertMatch(Guard,Expr),ok).
-else.
-define(assertMatch(Guard, Expr),
	((fun () ->
	    case (Expr) of
		Guard -> ok;
		__V -> erlang:error({assertMatch_failed, (??Expr),
				     {expected, (??Guard)},
				     {value, __V}})
	    end
	  end)())).
-endif.
-define(_assertMatch(Guard, Expr), ?_test(?assertMatch(Guard, Expr))).

%% Note: Class and Term are patterns, and can not be used for value.
-ifdef(NOTEST).
-define(assertException(Class, Term, Expr),ok).
-else.
-define(assertException(Class, Term, Expr),
	((fun () ->
	    try (Expr) of
	        __V -> erlang:error({assertException_failed, (??Expr),
				     {expected,(??Class)++":"++(??Term)},
				     {unexpected_success, __V}})
	    catch
		Class:Term -> ok;
	        __C:__T ->
		    erlang:error({assertException_failed, (??Expr),
				  {expected,(??Class)++":"++(??Term)},
				  {unexpected_exception,
				   {__C, __T, erlang:get_stacktrace()}}})
	    end
	  end)())).
-endif.

-define(assertError(Term, Expr), ?assertException(error, Term, Expr)).
-define(assertExit(Term, Expr), ?assertException(exit, Term, Expr)).
-define(assertThrow(Term, Expr), ?assertException(throw, Term, Expr)).

-define(_assertException(Class, Term, Expr),
	?_test(?assertException(Class, Term, Expr))).
-define(_assertError(Term, Expr), ?_assertException(error, Term, Expr)).
-define(_assertExit(Term, Expr), ?_assertException(exit, Term, Expr)).
-define(_assertThrow(Term, Expr), ?_assertException(throw, Term, Expr)).

%% Macros for running operating system commands. (Note that these
%% require EUnit to be present at runtime, or at least eunit_lib.)

%% these can be used for simply running commands in a controlled way
-define(_cmd_(Cmd), (eunit_lib:command(Cmd))).
-define(cmdStatus(N, Cmd),
	((fun () ->
	    case ?_cmd_(Cmd) of
		{(N), __Out} -> __Out;
		{__N, _} -> erlang:error({command_failed, (Cmd),
					  {expected_status,(N)},
					  {status,__N}})
	    end
	  end)())).
-define(_cmdStatus(N, Cmd), ?_test(?cmdStatus(N, Cmd))).
-define(cmd(Cmd), ?cmdStatus(0, Cmd)).
-define(_cmd(Cmd), ?_test(?cmd(Cmd))).

%% these are only used for testing; they always return 'ok' on success,
%% and have no effect if testing is turned off
-ifdef(NOTEST).
-define(assertCmdStatus(N, Cmd),ok).
-else.
-define(assertCmdStatus(N, Cmd),
 	((fun () ->
	    case ?_cmd_(Cmd) of
		{(N), _} -> ok;
		{__N, _} -> erlang:error({assertCmd_failed, (Cmd),
					  {expected_status,(N)},
					  {status,__N}})
	    end
	  end)())).
-define(assertCmdOutput(T, Cmd),
 	((fun () ->
	    case ?_cmd_(Cmd) of
		{_, (T)} -> ok;
		{_, __T} -> erlang:error({assertCmdOutput_failed, (Cmd),
					  {expected_output,(T)},
					  {output,__T}})
	    end
	  end)())).
-endif.
-define(assertCmd(Cmd), ?assertCmdStatus(0, Cmd)).

-define(_assertCmdStatus(N, Cmd), ?_test(?assertCmdStatus(N, Cmd))).
-define(_assertCmd(Cmd), ?_test(?assertCmd(Cmd))).
-define(_assertCmdOutput(T, Cmd), ?_test(?assertCmdOutput(T, Cmd))).

-endif. % EUNIT_HRL
