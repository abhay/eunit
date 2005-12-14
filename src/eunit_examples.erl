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
%%     $Id: eunit_examples.erl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% @copyright 2004,2005 Mickaël Rémond
%% @version 1.1, {@date} {@time}.
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @see eunit
%% @doc Eunit_examples is a simple unit test module. It illustrates
%% how to write eunit tests.
%% Running those tests implies: 2 tests passing and 1 test failing
%% <br/>
%% The tests are run by issuing the following command from the Erlang
%% shell:<br/>
%% <code>eunit:run([eunit_examples], {suffix, "_test"}).</code>
%% <br/>
%%
%% The result is the following:
%% <pre>
%% Erlang (BEAM) emulator version 5.3 [source] [hipe]
%%
%% Eshell V5.3  (abort with ^G)
%% 1> eunit:run([eunit_examples], {suffix,"_test"}).
%% Running unit tests in module [eunit_examples]
%% Running test eunit_examples:failed_test/0 - failed.
%% Running test eunit_examples:error_test/0 - passed.
%% Running test eunit_examples:standard_test/0 - passed.
%% Test finished
%%
%%
%% ========
%%   2 passed
%%   1 failed
%% ========
%%
%% {2,
%%  1,
%%  [{eunit_examples,{standard_test,0}},{eunit_examples,{error_test,0}}],
%%  [{eunit_examples,{failed_test,0}}]}
%% </pre>
%% Tests are functions whose name share a common prefix or
%% suffix. They take no arguments. This means that test functions are
%% always function of arity 0.
%% <br/>
%% Tests expectation are written as matching expression.
%% The expected result is on the left of the '=' pattern matching expression.
%% The right side of the expression is a term or a function call that
%% you expect a particular result from.
%% <br/>
%% For example, when you write a test as:
%% <pre>
%% predicate_test() ->
%%     true = predicate(zogzog).
%% </pre>
%% You expect the predicate/1 function to return true when passed the
%% atom <code>zogzog</code> as parameter
%% <br/>
%% If you want to benefit from helper macros when writing tests, you should include the eunit.hrl file:
%%  -include_lib("eunit/include/eunit.hrl").
%% @end

-module(eunit_examples).

-export([standard_test/0,
	 error_test/0,
	 failed_test/0,
	 not_a_eunit_test/1]).

-include("eunit.hrl").

%% @spec standard_test() -> ok
%% @doc
%% Test are written as normal match
%% @end
standard_test() ->
    ?log("Running standard test", []),
    ?match(1,1),
    ok.

%% @spec error_test() -> ok
%% @doc This is an example of a test triggering a "normal", expected
%% error.  When you are writing tests for case that should generate a
%% runtime error, you can check that the result of the call match to the Exit signal
%% 'EXIT', _}. This is a way to invert the test result and to expect
%% error from a given code. The test succeed if the error happens and
%% fails if the error does not occur.
%% @end
error_test() ->
    ?match({'EXIT',_}, 1 = 2),
    ok.

%% @spec failed_test() -> ok
%% @doc This is an example of failing test. This test always failed,
%% as the assertion is never verified (badmatch). Failed matches leads
%% to failed tests.
%% @end
failed_test() ->
    ?match(1, 2),
    ok.

%% @spec not_a_eunit_test(Param::term()) -> ok
%% @doc This is not a test primary function executed by eunit as arity
%% is not zero. Remember that not all exported functions are
%% considered as unit tests. Only functions matching suffix or prefix
%% pattern with an arity of zero are executed.
%% @end
not_a_eunit_test(Param) ->
    ok.
