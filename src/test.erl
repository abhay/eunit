%% -------------------------------------------------------------------
%% File: test.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richard@virtutech.com>
%% @copyright 2005 Richard Carlsson
%% @private

-module(test).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

-include_lib("eunit/include/eunit_test.hrl").

succeed() ->
    ok.

fail() ->
    throw(failed).

succeeding_test() ->
    succeed().

failing_test() ->
    fail().

succeeding_fun_test_() ->
    fun () -> succeed() end.

failing_fun_test_() ->
    fun () -> fail() end.

succeeding_simple_test_() ->
    ?_test(succeed()).

failing_simple_test_() ->
    ?_test(fail()).

succeeding_assert_test_() ->
    ?_assert(1 > 0).

failing_assert_test_() ->
    ?_assert(0 > 1).

succeeding_error_test_() ->
    ?_assertError(foo, erlang:error(foo)).

failing_error_test_() ->
    ?_assertError(foo, erlang:throw(foo)).

succeeding_exit_test_() ->
    ?_assertExit(foo, erlang:exit(foo)).

failing_exit_test_() ->
    ?_assertExit(foo, erlang:throw(foo)).

succeeding_throw_test_() ->
    ?_assertThrow(foo, erlang:throw(foo)).

failing_throw_test_() ->
    ?_assertThrow(foo, erlang:exit(foo)).

succeeding_named_test_() ->
    {"a Test with a Title",
     ?_test(succeed())}.

failing_named_test_() ->
    {"a Failing Test with a Title",
     ?_test(fail())}.

succeeding_wrapper_test_() ->
    {"a Wrapped Function",
     fun succeeding_test/0}.

failing_wrapper_test_() ->
    {"a failing Wrapped Function",
     fun failing_test/0}.

empty_list_test_() ->
    []. %% should be accepted, but will not produce any visible result

list_test_() ->
    [?_test(succeed()),
     ?_test(fail()),
     ?_test(succeed())].

deep_list_test_() ->
    [?_test(succeed()),
     [[],
      ?_test(fail())],
     ?_test(succeed()),
     []].

group_test_() ->
    {"a Group of Tests",
     [?_test(succeed()),
      ?_test(fail()),
      ?_test(succeed())]
    }.

list_of_named_test_() ->
    [{"List Test One", ?_test(succeed())},
     {"List Test Two", ?_test(fail())},
     {"List Test Three", ?_test(succeed())}].

named_group_test_() ->
    {"a Group of Tests with Titles",
     [{"Subtest One", ?_test(succeed())},
      {"Subtest Two", ?_test(fail())},
      {"Subtest Three", ?_test(succeed())}]
    }.

nested_named_list_test_() ->
    {"Named List Level One",
     [?_test(succeed()),
      {"Named List Level Two",
       [?_test(succeed()),
	?_test(fail()),
	?_test(succeed())]},
      ?_test(fail()),
      ?_test(succeed())]
    }.

higher_order_test_() ->
    {"Higher Order Tests",
     [
      {"Setup Test",
       {setup,
	fun () -> 4711 end,
	fun (4711) -> ok end,
	fun (X) ->
		[{"1st", ?_assert(X == 4711)},
		 {"2nd", ?_assert(X == 4711)},
		 {"3rd", ?_assert(X == 4711)}]
	end}
      },
      {"Foreach Test",
       {foreach,
	fun () -> 4711 end,
	fun (4711) -> ok end,
	[fun (X) -> {"1st", ?_assert(X == 4711)} end,
	 [fun (X) -> {"2nd", ?_assert(X == 4711)} end],
	 fun (X) -> {"3rd", ?_assert(X == 4711)} end]
       }
      },
      {"Foreach1 Test",
       {foreach1,
	fun (X) -> 4711 + X end,
	fun (A, X) when X - A == 4711 -> ok end,
	[{1, fun (A, X) -> {"1st",
			    ?_assert((A == 1) and (X == 4712))}
	     end},
	 [{2, fun (A, X) -> {"2nd",
			     ?_assert((A == 2) and (X == 4713))}
	      end}],
	 {3, fun (A, X) -> {"3rd",
			    ?_assert((A == 3) and (X == 4714))}
	     end}
	]}
      }
     ]
    }.

%% setup/foreach usage examples

file_setup(Name, Mode) ->
    {ok, FD} = file:open(Name, Mode),
    FD.

file_cleanup(FD) ->
    file:close(FD).

make_file_setup(Name, Mode) ->
    fun () -> file_setup(Name, Mode) end.

file_test(FD, FileName, Text) ->
    {Text, ?_test(msg(FD, FileName, Text))}.

msg(FD, FileName, Text) ->
    io:fwrite(FD, "~s: ~s\n", [FileName, Text]).

setup_test_() ->
    File = "setup_test.txt",
    {setup,
     make_file_setup(File, [write]),
     fun file_cleanup/1,
     abstract_file_test("hello!", "setup_test")}.

abstract_file_test(Text, FileName) ->
    fun (FD) -> file_test(FD, FileName, Text) end.

foreach_test_() ->
    File = "foreach_test.txt",
    {foreach,
     make_file_setup(File, [append]),
     fun file_cleanup/1,
     [abstract_file_test("one", File),
      abstract_file_test("two", File),
      abstract_file_test("three", File),
      abstract_file_test("four", File)
     ]}.

more_abstract_file_test(Text) ->
    fun (FileName, FD) -> (abstract_file_test(Text, FileName))(FD) end.

foreach1_test_() ->
    {foreach1,
     fun (File) -> file_setup(File, [append]) end,
     fun (_File, FD) -> file_cleanup(FD) end,
     [{"foreach1_test_a.txt", more_abstract_file_test("eins")},
      {"foreach1_test_b.txt", more_abstract_file_test("zwei")},
      {"foreach1_test_c.txt", more_abstract_file_test("drei")},
      {"foreach1_test_d.txt", more_abstract_file_test("vier")}
     ]}.

order_test_() ->
    {inparallel,
     [{inorder,
       [{"put 42",	?_test(undefined = put(foo, 42))},
	{"get 42",	?_test(42 = get(foo))}
       ]},
      {inorder,
       [{"put 17",	?_test(undefined = put(foo, 17))},
	{"get 17",	?_test(17 = get(foo))}
       ]},
      {inorder,
       [{"put 4711",	?_test(undefined = put(foo, 4711))},
	{"fail", ?_test(exit(foo))},
	{"get 4711",	?_test(4711 = get(foo))}
       ]}
     ]}.

spawn_test_() ->
    {"Level One",
     inorder,
     [{"put 42",	?_test(undefined = put(foo, 42))},
      {"Level two",
       spawn,
       [{"put 17",	?_test(undefined = put(foo, 17))},
	{"Level three",
	 spawn,
	 [{"put 4711",	?_test(undefined = put(foo, 4711))},
	  {"fail", ?_test(exit(foo))},
	  {"get 4711",	?_test(4711 = get(foo))}
	 ]},
	{"get 17",	?_test(17 = get(foo))}
       ]},
      {"get 42",	?_test(42 = get(foo))}
     ]}.
