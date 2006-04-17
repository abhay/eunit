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
%% File: eunit_text.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

%% @TODO less verbose mode by default

-module(eunit_tty).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1]).

%% EUnit self-testing 
-include("eunit_test.hrl").


-record(state, {succeed = 0,
		fail = 0,
		cancel = 0,
		indent = 0}).

start(List) ->
    St = #state{},
    Id = [],
    spawn(fun () -> init(Id, List, St) end).

init(Id, List, St0) ->
    io:fwrite("=== EUnit ===\n"),
    St = group_begin(Id, "", List, St0),
    ?debugmsg1("top list done; waiting for stop message: ~w", [St]),
    %% @TODO report and if possible count aborted tests!
    receive
	{stop, ReplyTo, Reference} ->
	    io:fwrite("================================\n"
		      "  Failed: ~w.  Succeeded: ~w.\n",
		      [St#state.fail, St#state.succeed]),
	    Result = if St#state.fail > 0 -> error;
			true -> ok
		     end,
	    ReplyTo ! {result, Reference, Result},
	    ok
    end.

wait(Id, St) ->
    receive
	{status, Id, Data} -> {Data, St}
    end.

entry({item, Id, Desc, Test}, St) ->
    test_begin(Id, Desc, Test, St);
entry({group, Id, Desc, Es}, St) ->
    group_begin(Id, Desc, Es, St).

tests([E | Es], St) ->
    tests(Es, entry(E, St));
tests([], St) ->
    St.

test_begin(Id, Desc, {Module, Name}, St) ->
    test_begin(Id, Desc, {Module, Name, 0}, St);
test_begin(Id, Desc, {Module, Name, Line}, St) ->
    print_test_begin(St#state.indent, Module, Name, Line, Desc),
    case wait(Id, St) of
	{{progress, {'begin', test}}, St1} ->
	    test_end(Id, St1);
	{{cancel, Reason}, St1} ->
	    print_test_cancel(Reason),
	    St1#state{cancel = St1#state.cancel + 1}
    end.

test_end(Id, St) ->
    case wait(Id, St) of
	{{progress, {'end', {Result, Time}}}, St1} ->
	    if Result == ok ->
		    print_test_end(Time),
		    St1#state{succeed = St1#state.succeed + 1};
	       true ->
		    print_test_error(Result),
		    St1#state{fail = St1#state.fail + 1}
	    end;
	{{cancel, Reason}, St1} ->
	    print_test_cancel(Reason),
	    St1#state{cancel = St1#state.cancel + 1}
    end.

group_begin(Id, Desc, Es, St0) ->
    I = St0#state.indent,
    St = if Desc == "" -> St0;
	    true ->
		 print_group_start(I, Desc),
		 St0#state{indent = I + 1}
	 end,
    case wait(Id, St) of
	{{progress, {'begin', group}}, St1} ->
	    group_end(Id, I, tests(Es, St1));
	{{cancel, Reason}, St1} ->
	    if Desc == "" -> ok;
	       true ->
		    print_group_cancel(I, Reason)
	    end,
	    St1#state{indent = I}
    end.

group_end(Id, I, St) ->
    (case wait(Id, St) of
	 {{progress, {'end', {ok, Time}}}, St1} ->
	     print_group_end(St1#state.indent, Time),
	     St1;
	 {{cancel, undefined}, St1} ->
	     St1;  %% "skipped" message is not interesting here
	 {{cancel, _Reason}, St1} ->
	     print_group_cancel(I, _Reason),
	     St1
     end)#state{indent = I}.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.

print_group_start(I, Desc) ->
    indent(I),
    io:fwrite("~s\n", [Desc]).

print_group_end(I, Time) ->
    if Time > 0 ->
	    indent(I),
	    io:fwrite("[group done in ~.3f s]\n", [Time/1000]);
       true ->
	    ok
    end.

print_test_begin(I, Module, Name, Line, Desc) ->
    indent(I),
    L = if Line == 0 -> "";
	   true -> io_lib:fwrite("~w:", [Line])
	end,
    D = if Desc == "" -> "";
	   true -> io_lib:fwrite(" (~s)", [Desc])
	end,
    io:fwrite("~s:~s~s~s...", [Module, L, Name, D]).

print_test_end(Time) ->
    T = if Time > 0 -> io_lib:fwrite("[~.3f s] ", [Time/1000]);
	   true -> ""
	end,
    io:fwrite("~sok\n", [T]).

print_test_error({error, Exception}) ->
    io:fwrite("*failed*\n::~p\n\n", [Exception]);
print_test_error({skipped, Reason}) ->
    io:fwrite("*did not run*\n::~s\n\n",
	      [format_skipped(Reason)]).

format_skipped({module_not_found, M}) ->
    io_lib:format("missing module: ~w", [M]);
format_skipped({no_such_function, {M,F,A}}) ->
    io_lib:format("no such function: ~w:~w/~w", [M,F,A]).    

print_test_cancel(Reason) ->
    io:fwrite(format_cancel(Reason)).

print_group_cancel(_I, {blame, _}) ->
    ok;
print_group_cancel(I, Reason) ->
    indent(I),
    io:fwrite(format_cancel(Reason)).

format_cancel(undefined) ->
    "*skipped*\n";
format_cancel(timeout) ->
    "*timed out*\n";
format_cancel({startup, Reason}) ->
    io_lib:fwrite("*could not start test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({blame, _SubId}) ->
    "*cancelled because of subtask*\n";
format_cancel({exit, Reason}) ->
    io_lib:fwrite("*unexpected termination of test process*\n::~P\n\n",
		  [Reason, 15]);
format_cancel({abort, Reason}) ->
    aborted(Reason).

aborted(Reason) ->
    case Reason of
	{setup_failed, Exception} ->
	    cancel_msg("context setup failed", "~p", [Exception]);
 	{cleanup_failed, Exception} ->
 	    cancel_msg("context cleanup failed", "~p", [Exception]);
 	{instantiation_failed, Exception} ->
 	    cancel_msg("instantiation of subtests failed", "~p",
		       [Exception]);
 	{bad_test, Bad} ->
 	    cancel_msg("bad test descriptor", "~p", [Bad]);
 	{no_such_function, {M,F,A}} ->
 	    cancel_msg(io_lib:format("no such function: ~w:~w/~w", [M,F,A]),
		      "", []);
 	{module_not_found, M} ->
 	    cancel_msg("test module not found", "~p", [M]);
 	{generator_failed, Exception} ->
 	    cancel_msg("test generator failed", "~p", [Exception])
    end.

cancel_msg(Title, Str, Args) ->
    Msg = io_lib:format(Str, Args),
    io_lib:fwrite("*** ~s ***\n::~s\n\n", [Title, Msg]).
