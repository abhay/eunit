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
%% The Initial Developer of the Original Code is Mickaël Rémond.''
%%
%%     $Id: eunit.hrl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%

%% Since this file is normally included with include_lib, it must in its
%% turn use include_lib to read any other header files, at least until
%% the epp include_lib behaviour is fixed.
-include_lib("eunit/include/eunit_old.hrl").
%%-include("eunit_old.hrl").

-define(_test(Expr), {?LINE, fun () -> (Expr), ok end}).
-define(_test1(Str, Expr), {Str, ?_test(Expr)}).

-define(_assert(BoolExpr),
	?_test(case (BoolExpr) of
		   true -> ok;
		   false -> throw(assertion_failed)
	       end)).
-define(_assert1(Str, BoolExpr), {Str, ?_assert(BoolExpr)}).

-define(_assertNot(BoolExpr), ?_assert(not (BoolExpr))).
-define(_assertNot1(Str, BoolExpr), {Str, ?_assertNot(BoolExpr)}).

%% Class and Term may be patterns here
-define(_assertException(Class, Term, Expr),
	?_test(try (Expr) of
		   Value -> throw({unexpected_success, Value})
	       catch
		   Class:Term -> ok;
		   Class1:Term1 ->
		       throw({unexpected_exception,
			      {Class1, Term1, erlang:get_stacktrace()}})
	       end)).
-define(_assertError(Term, Expr), ?_assertException(error, Term, Expr)).
-define(_assertExit(Term, Expr), ?_assertException(exit, Term, Expr)).
-define(_assertThrow(Term, Expr), ?_assertException(throw, Term, Expr)).
