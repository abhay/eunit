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


-define(AUTO_TIMEOUT, 60000).   %% auto test time limit


start(Server) when is_atom(Server) ->
    ensure_started(Server).

stop(Server) ->
    command(Server, stop).


-record(job, {super, test, options}).

start_test(Server, Super, T, Options) ->
    command(Server, {start, #job{super = Super,
				 test = T,
				 options = Options}}).

%% @TODO: add test options to watch mechanism
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

-record(state, {name,
		stopped,
		jobs,
		queue,
		auto_test,
		watch_modules,
		watch_paths,
		watch_regexps}).

server_init(Name) ->
    server_loop(#state{name = Name,
		       stopped = false,
		       jobs = dict:new(),
		       queue = queue:new(),
		       auto_test = queue:new(),
		       watch_modules = sets:new(),
		       watch_paths = sets:new(),
		       watch_regexps = sets:new()}).

server_loop(St) ->
    server_check_exit(St),
    receive
	{done, auto_test, _Pid} ->
	    server_loop(auto_test_done(St));
	{done, Reference, _Pid} ->
	    server_loop(handle_done(Reference, St));
	{command, From, _Cmd} when St#state.stopped ->
	    From ! {self(), stopped};
	{command, From, Cmd} ->
	    server_command(From, Cmd, St);
	{code_monitor, {loaded, M}} ->
	    case is_watched(M, St) of
		true -> 
		    server_loop(new_auto_test(self(), M, St));
		false ->
		    server_loop(St)
	    end
    end.

server_check_exit(St) ->
    case dict:size(St#state.jobs) of
	0 when St#state.stopped -> exit(normal);
	_ -> ok
    end.

server_command(From, {start, Job}, St) ->
    Reference = make_ref(),
    St1 = case proplists:get_bool(enqueue, Job#job.options) of
	      true ->
		  enqueue(Job, From, Reference, St);
	      false ->
		  start_job(Job, From, Reference, St)
	  end,
    server_command_reply(From, {ok, Reference}),
    server_loop(St1);
server_command(From, stop, St) ->
    %% unregister the server name and let remaining jobs finish
    server_command_reply(From, {error, stopped}),
    catch unregister(St#state.name),
    server_loop(St#state{stopped = true});
server_command(From, {watch, Target}, St) ->
    %% the code watcher is only started on demand
    code_monitor:monitor(self()),
    St1 = add_watch(Target, St),
    server_command_reply(From, ok),
    server_loop(St1);
server_command(From, {forget, Target}, St) ->
    St1 = delete_watch(Target, St),
    server_command_reply(From, ok),
    server_loop(St1);
server_command(From, Cmd, St) ->
    server_command_reply(From, {error, {unknown_command, Cmd}}),
    server_loop(St).

server_command_reply(From, Result) ->
    From ! {self(), Result}.

enqueue(Job, From, Reference, St) ->
    case dict:size(St#state.jobs) of
	0 ->
	    start_job(Job, From, Reference, St);
	_ ->
	    St#state{queue = queue:in({Job, From, Reference},
				      St#state.queue)}
    end.

dequeue(St) ->
    case queue:out(St#state.queue) of
	{empty, _} ->
	    St;
	{{value, {Job, From, Reference}}, Queue} ->
	    start_job(Job, From, Reference, St#state{queue = Queue})
    end.

start_job(Job, From, Reference, St) ->
    From ! {start, Reference},
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_value(order, Job#job.options, inorder),
    eunit_proc:start(Job#job.test, Order, Job#job.super, Reference),
    St#state{jobs = dict:store(Reference, From, St#state.jobs)}.

handle_done(Reference, St) ->
    case dict:find(Reference, St#state.jobs) of
	{ok, From} ->
	    From ! {done, Reference},
	    dequeue(St#state{jobs = dict:erase(Reference,
					       St#state.jobs)});
	error ->
	    St
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

%% Running automatic tests when a watched module is loaded.
%% Uses a queue in order to avoid overlapping output when several
%% watched modules are loaded simultaneously. (The currently running
%% automatic test is kept in the queue until it is done. An empty queue
%% means that no automatic test is running.)

new_auto_test(Server, M, St) ->
    case queue:is_empty(St#state.auto_test) of
	true ->
	    start_auto_test(Server, M);
	false ->
	    ok
    end,
    St#state{auto_test = queue:in({Server, M}, St#state.auto_test)}.

auto_test_done(St) ->
    %% remove finished test from queue before checking for more
    {_, Queue} = queue:out(St#state.auto_test),
    case queue:out(Queue) of
	{{value, {Server, M}}, _} ->
	    %% this is just lookahead - the item is not removed
	    start_auto_test(Server, M);
	{empty, _} ->
	    ok
    end,
    St#state{auto_test = Queue}.

start_auto_test(Server, M) ->
    spawn(fun () -> auto_super(Server, M) end).

auto_super(Server, M) ->
    process_flag(trap_exit, true),
    %% Give the user a short delay before any output is produced
    receive after 333 -> ok end,
    %% Make sure output is sent to console on server node
    group_leader(whereis(user), self()),
    Pid = spawn_link(fun () -> auto_proc(Server, M) end),
    receive
	{'EXIT', Pid, _} ->
	    ok
    after ?AUTO_TIMEOUT	->
	    exit(Pid, kill),
	    io:put_chars("\n== EUnit: automatic test was aborted ==\n"),
	    io:put_chars("\n> ")
    end,
    Server ! {done, auto_test, self()}.

auto_proc(Server, M) ->
    %% Make the output start on a new line instead of on the same line
    %% as the current shell prompt.
    io:fwrite("\n== EUnit: testing module ~w ==\n", [M]),
    eunit:test(Server, M, [enqueue]),
    %% Make sure to print a dummy prompt at the end of the output, most
    %% of all so that the Emacs mode realizes that input is active.
    io:put_chars("\n-> ").
