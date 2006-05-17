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
%% @doc EUnit server process

-module(eunit_server).

-export([start/1, stop/1, start_test/4, watch/2, watch_path/2,
	 watch_regexp/2]).

-include("eunit.hrl").
-include("eunit_internal.hrl").


start(Server) when is_atom(Server) ->
    ensure_started(Server).

stop(Server) ->
    command(Server, stop).

start_test(Server, Super, T, Options) ->
    command(Server, {test, Super, T, Options}).

watch(Server, Module) when is_atom(Module) ->
    command(Server, {watch, {module, Module}}).

watch_path(Server, Path) ->
    command(Server, {watch, {path, filename:flatten(Path)}}).

watch_regexp(Server, Regex) ->
    case regexp:parse(Regex) of
	{ok, R} ->
	    command(Server, {watch, {regexp, R}});
	{error, _}=Error ->
	    Error
    end.

%% This makes sure the server is started before sending the command, and
%% returns {ok, Result} if the server accepted the command or {error,
%% server_down} if the server process crashes. If the server does not
%% reply, this function will wait until the server is killed.

command(Server, Cmd) ->
    if is_atom(Server), Cmd /= stop -> ensure_started(Server);
       true -> ok
    end,
    if is_pid(Server) -> command_1(Server, Cmd);
       true ->
	    case whereis(Server) of
		undefined -> {error, server_down};
		Pid -> command_1(Pid, Cmd)
	    end
    end.

command_1(Pid, Cmd) when is_pid(Pid) ->
    Pid ! {command, self(), Cmd},
    command_wait(Pid, 1000, undefined).

command_wait(Pid, Timeout, Monitor) ->
    receive
	{Pid, Result} -> Result;
	{'DOWN', Monitor, process, Pid, _R} -> {error, server_down}
    after Timeout ->
	    %% avoid creating a monitor unless some time has passed
	    command_wait(Pid, infinity, erlang:monitor(process, Pid))
    end.

%% Starting the server

ensure_started(Name) ->
    ensure_started(Name, 5).

ensure_started(Name, N) when N > 0 ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_start(Name, Parent) end),
	    receive
		{Pid, ok} ->
		    Pid;
		{Pid, error} ->
		    receive after 200 -> ensure_started(N - 1) end
	    end;
	Pid ->
	    Pid
    end;
ensure_started(_, _) ->
    throw(no_server).

server_start(Name, Parent) ->
    Pid = self(),
    try register(Name, Pid) of
	true ->
	    Parent ! {Pid, ok},
	    server_init(Name)
    catch
	_:_ ->
	    Parent ! {Pid, error},
	    exit(error)
    end.

-record(state, {name, stopped, watch_modules, watch_paths, watch_regexps}).

server_init(Name) ->
    server_loop(dict:new(), #state{stopped = false,
				   name = Name,
				   watch_modules = sets:new(),
				   watch_paths = sets:new(),
				   watch_regexps = sets:new()}).

server_loop(Jobs, St) ->
    server_check_exit(Jobs, St),
    receive
	{done, Reference, _Pid} ->
	    server_loop(handle_done(Reference, Jobs), St);
	{command, From, _Cmd} when St#state.stopped ->
	    From ! {self(), stopped};
	{command, From, Cmd} ->
	    server_command(From, Cmd, Jobs, St);
	{code_monitor, {loaded, M}} ->
	    case is_watched(M, St) of
		true -> start_auto_test(M);
		false -> ok
	    end,
	    server_loop(Jobs, St)
    end.

server_check_exit(Jobs, St) ->
    case dict:size(Jobs) of
	0 when St#state.stopped -> exit(normal);
	_ -> ok
    end.

server_command(From, {test, Super, T, Options}, Jobs, St) ->
    Reference = start_test(T, Options, Super),
    server_command_reply(From, {ok, Reference}),
    server_loop(dict:store(Reference, From, Jobs), St);
server_command(From, stop, Jobs, St) ->
    %% unregister the server name and let remaining jobs finish
    server_command_reply(From, {error, stopped}),
    catch unregister(St#state.name),
    server_loop(Jobs, St#state{stopped = true});
server_command(From, {watch, Target}, Jobs, St) ->
    %% the code watcher is only started on demand
    code_monitor:monitor(self()),
    St1 = add_watch(Target, St),
    server_command_reply(From, ok),
    server_loop(Jobs, St1);
server_command(From, {forget, Target}, Jobs, St) ->
    St1 = delete_watch(Target, St),
    server_command_reply(From, ok),
    server_loop(Jobs, St1);
server_command(From, Cmd, Jobs, St) ->
    server_command_reply(From, {error, {unknown_command, Cmd}}),
    server_loop(Jobs, St).

server_command_reply(From, Result) ->
    From ! {self(), Result}.

start_test(T, Options, Super) ->
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_value(order, Options, inorder),
    {Reference, _Pid} = eunit_proc:start(T, Order, Super),
    Reference.

handle_done(Reference, Jobs) ->
    case dict:find(Reference, Jobs) of
	{ok, Pid} ->
	    Pid ! {done, Reference},
	    dict:erase(Reference, Jobs);
	error ->
	    Jobs
    end.

%% Adding and removing watched modules or paths

add_watch({module, M}, St) ->
    St#state{watch_modules =
	     sets:add_element(M, St#state.watch_modules)};
add_watch({path, P}, St) ->
    St#state{watch_paths =
	     sets:add_element(P, St#state.watch_paths)};
add_watch({regexp, R}, St) ->
    St#state{watch_regexps =
	     sets:add_element(R, St#state.watch_regexps)}.

delete_watch({module, M}, St) ->
    St#state{watch_modules =
	     sets:del_element(M, St#state.watch_modules)};
delete_watch({path, P}, St) ->
    St#state{watch_paths =
	     sets:del_element(P, St#state.watch_paths)};
delete_watch({regexp, R}, St) ->
    St#state{watch_paths =
	     sets:del_element(R, St#state.watch_regexps)}.

%% Checking if a module is being watched

is_watched(M, St) when is_atom(M) ->
    sets:is_element(M, St#state.watch_modules) orelse
	is_watched(filename:dirname(code:which(M)), St);
is_watched(Path, St) ->
    sets:is_element(Path, St#state.watch_paths) orelse
	match_any(sets:to_list(St#state.watch_regexps), Path).

match_any([R | Rs], Path) ->
    case regexp:first_match(Path, R) of
	{match, _, _} -> true;
	_ -> match_any(Rs, Path)
    end;
match_any([], _P) -> false.

%% Process for running automatic tests when a watched module is loaded.

start_auto_test(M) ->
    spawn(fun () -> auto_test(M) end).

auto_test(M) ->
    group_leader(whereis(user), self()),
    receive after 800 -> ok end,
    %% Make the output start on a new line instead of on the same line
    %% as the current shell prompt.
    io:fwrite("\n== EUnit: testing module ~w ==\n", [M]),
    %% Make sure to print a dummy prompt at the end of the output, most
    %% of all so that the Emacs mode realizes that input is active.
    io:fwrite("~w\n> ", [eunit:test(M)]).
