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
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

%% @TODO cleanup and define the protocol for frontend processes

-module(eunit_text).

-export([start/1]).

-record(state, {ref,
		succeed = 0,
		fail = 0,
		abort = false,
		indent = 0}).

start(Reference) ->
    St = #state{ref = Reference},
    spawn_link(fun () -> init(St) end).

init(St) ->
    io:fwrite("=== EUnit ===\n"),
    loop(St).

loop(St) ->
    Reference = St#state.ref,
    receive
	{Reference, stop, ReplyTo} ->
	    io:fwrite("================================\n"
		      "  Failed: ~w.  Succeeded: ~w.\n",
		      [St#state.fail, St#state.succeed]),
	    if St#state.abort ->
		    io:fwrite("\n*** the testing was "
			      "prematurely aborted *** \n");
	       true ->
		    ok
	    end,
	    Result = if (St#state.fail > 0) or St#state.abort -> error;
			true -> ok
		     end,
	    ReplyTo ! {Reference, result, Result},
	    ok;
	{test, start, {Module, Line, Name, Desc}} ->
	    indent(St#state.indent),
	    io:fwrite("~s:~s~s~s...",
		      [Module, 
		       case Line of
			   0 -> "";
			   L -> io_lib:fwrite("~w:", [L])
		       end,
		       Name,
		       if Desc == undefined -> "";
			  true -> io_lib:fwrite(" (~s)", [Desc])
		       end]),
	    loop(St);
	{test, done, ok} ->
	    io:fwrite("ok\n"),
	    St1 = St#state{succeed = St#state.succeed + 1},
	    loop(St1);
	{test, done, {error, Exception}} ->
	    io:fwrite("*failed*\n::~p\n\n", [Exception]),
	    St1 = St#state{fail = St#state.fail + 1},
	    loop(St1);
	{test, done, {skipped, Reason}} ->
	    St1 = St#state{fail = St#state.fail + 1},
	    skipped(Reason),
	    loop(St1);
	{group, enter, Desc} ->
	    case Desc of
		undefined -> ok;
		Desc ->
		    indent(St#state.indent),
		    io:fwrite(Desc),
		    io:nl()
	    end,
	    St1 = St#state{indent = St#state.indent + 1},
	    loop(St1);
	{group, leave, _Desc} ->
	    St1 = St#state{indent = St#state.indent - 1},
	    loop(St1);
	{abort, Reason} ->
	    St1 = St#state{abort = true},
	    aborted(Reason),
	    loop(St1)
    end.

skipped(Reason) ->
    case Reason of
	{module_not_found, M} ->
	    skip_msg("missing module: ~w", [M]);
	{no_such_function, {M,F,A}} ->
	    skip_msg("no such function: ~w:~w/~w", [M,F,A]);
	{error, Exception} ->
	    skip_msg("internal error: ~p", [Exception])
    end.

skip_msg(Str, Args) ->
    Msg = io_lib:format(Str, Args),
    io:fwrite("** did not run **\n::~s\n\n", [Msg]).

aborted(Reason) ->
    case Reason of
	setup_failed ->
	    abort_msg("context setup failed", "", []);
	cleanup_failed ->
	    abort_msg("context cleanup failed", "", []);
	instantiation_failed ->
	    abort_msg("instantiation of subtests failed", "", []);
	{bad_test, Bad} ->
	    abort_msg("bad test descriptor", "~p", [Bad]);
	{no_such_function, {M,F,A}} ->
	    abort_msg(io_lib:format("no such function: ~w:~w/~w", [M,F,A]),
			"", []);
	{module_not_found, M} ->
	    abort_msg("test module not found", "~p", [M]);
	{generator_failed, Exception} ->
	    abort_msg("test generator failed", "~p", [Exception]);
	{terminated, Reason} ->
	    abort_msg("abnormal termination", "~p", [Reason])
    end.

abort_msg(Title, Str, Args) ->
    Msg = io_lib:format(Str, Args),
    io:fwrite("\n*** ~s ***\n::~s\n\n", [Title, Msg]).

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.
