%% -------------------------------------------------------------------
%% File: unit.hrl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richard@virtutech.com>
%% @copyright 2005 Richard Carlsson
%% @doc 

-define(_test(Expr), {?LINE, fun () -> (Expr), ok end}).
-define(_test1(Str, Expr), {Str, ?_test(Expr)}).

-define(_assert(BoolExpr),
	?_test(case (BoolExpr) of
		   true -> ok;
		   false -> throw(assertion_failed)
	       end)).
-define(_assert1(Str, BoolExpr), {Str, ?_assert(BoolExpr)}).

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
