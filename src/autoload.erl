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
%% @doc Erlang automatic code loading service

-module(autoload).

-export([start/0, start/1, stop/0, stop/1,
	 watch_module/1, watch_module/2,
	 watch_file/1, watch_file/2,
	 watch_dir/1, watch_dir/2
	]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, autoload).
-define(ZERO_TIMESTAMP, {0,0,0}).


%% We're not trying to provide strong guarantees here. The autoloader is
%% supposed to behave like a human assistant might - race conditions may
%% cause modules to be reloaded twice, or cause a reload from the wrong
%% file (if paths are suddenly changed).


watch_dir(Path) ->
    watch_dir(?SERVER, Path).

watch_dir(Server, Path) ->
    command(Server, {watch, {dir, filename:flatten(Path)}}).

watch_file(Path) ->
    watch_file(?SERVER, Path).

watch_file(Server, Path) ->
    command(Server, {watch, {file, filename:flatten(Path)}}).

watch_module(Module) ->
    watch_module(?SERVER, Module).

watch_module(Server, Module) when is_atom(Module) ->
    command(Server, {watch, {module, Module}}).


command(Server, Msg) ->
    ServerPid = ensure_started(Server),
    ServerPid ! {command, self(), Msg},
    receive
	{ServerPid, Result} -> Result
    end.

stop() ->
    stop(?SERVER).

stop(Server) ->
    Server ! stop,
    ok.
	    
ensure_started(Name) when is_atom(Name) ->
    start(Name);
ensure_started(Pid) when is_pid(Pid) ->
    Pid.


start() ->
    start(?SERVER).

start(Name) ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_init(Name, Parent) end),
	    receive
		{Pid, ok} -> Pid;
		{Pid, error} -> throw(no_server)
	    end;
	Pid -> Pid
    end.

-record(state, {name, modules}).

-record(module, {time = ?ZERO_TIMESTAMP, file}).

server_init(Name, Parent) ->
    Self = self(),
    case catch register(Name, Self) of
	true ->
	    Parent ! {Self, ok},
	    code_monitor:monitor(self()),
	    server(#state{name = Name,
			  modules = dict:new()});
	_ ->
	    init_failure(Parent)
    end.

init_failure(Parent) ->
    Parent ! {self(), error},
    exit(failed).

server(St) ->
    receive
	{file_monitor, _Ref, Msg} ->
	    file_event(Msg, St);
	{code_monitor, Msg} ->
	    code_event(Msg, St);
	{command, From, Cmd} ->
	    server_command(From, Cmd, St);
	stop ->
	    exit(normal);
	Msg ->
	    erlang:display({autoload_unexpected, Msg}),
	    server(St)
    end.

server_command_reply(From, Msg) ->
    From ! {self(), Msg}.

server_command(From, {watch, {dir, Path}}, St) ->
    server_command_reply(From, ok),
    monitor_dir(Path),
    server(St);
server_command(From, {watch, {file, Path}}, St) ->
    server_command_reply(From, ok),
    monitor_file(Path),
    server(St);
server_command(From, {watch, {module, M}}, St) ->
    server_command_reply(From, ok),
    server(handle_watch_module(M, St)).

file_event({exists, Path, dir, _Info, Files}, St) ->
    %%erlang:display({autoload_saw_exists, dir, Path}),
    monitor_beams(Path, Files),
    server(St);
file_event({exists, Path, file, Info, _}, St) ->
    %%erlang:display({autoload_saw_exists, file, Path}),
    %% treat file-exists messages just like file-changed messages
    server(changed_file(Path, Info#file_info.mtime, St));
file_event({changed, Path, dir, _Info, Files}, St) ->
    %%erlang:display({autoload_saw_changed, dir, Path}),
    monitor_beams(Path, Files),
    server(St);
file_event({changed, Path, file, #file_info{}=Info, _}, St) ->
    %%erlang:display({autoload_saw_changed, file, Path}),
    server(changed_file(Path, Info#file_info.mtime, St));
file_event({error, _Path, file, _ErrorAtom}=_Msg, St) ->
    %%erlang:display({autoload_ignoring, _Msg}),
    %% just consider the files as being temporarily unavailable
    server(St);
file_event(_Msg, St) ->
    erlang:display({autoload_unexpected_file_event, _Msg}),
    server(St).

code_event({loaded, M, Time}, St) ->
    %%erlang:display({autoload_saw_loaded, M, Time}),
    server(loaded_module(M, Time, St));
code_event(_Msg, St) ->
    %%erlang:display({autoload_unexpected_code_event, _Msg}),
    server(St).


%% called when asked to watch a module M

handle_watch_module(M, St) ->
    case code:which(M) of
	non_existing ->
	    %% no module loaded or found in path (e.g., the user has not
	    %% run 'make' yet, or has not updated the path): store
	    %% mapping from M to undefined file, for late binding
	    store_record(M, #module{}, St);
	File when is_list(File) ->
	    %% existing file found (already loaded or in path): monitor
	    %% the file and map M to that file (even if the first load
	    %% might turn out to be from another file)
	    monitor_file(File),
	    store_record(M, #module{file = File}, St);
	_ ->
	    %% preloaded or cover-compiled module - ignore
	    St
    end.


%% called when a monitored directory is detected as new or changed

monitor_beams(Path, Files) ->
    Beams = [F || {added, F} <- Files, filename:extension(F) == ".beam"],
    lists:foreach(
      fun (F) ->
	      F1 = filename:absname(filename:join(Path, F)),
	      %%erlang:display({autoload_monitoring, file, F1}),
	      monitor_file(F1)
      end,
      Beams).


%% called when a module was detected as loaded or reloaded (either
%% caused by an auto-load or by someone else)

loaded_module(M, Time, St) ->
    case find_record(M, St) of
	{ok, R} ->
	    File = code:which(M),
	    case R#module.file of
		undefined when is_list(File) ->
		    %% a watched module gets a late binding to a file
		    %%erlang:display({autoload_late_binding, M, File}),
		    monitor_file(File),
		    update_record(M, R, Time, File, St);
		File when is_list(File) ->
		    %% loaded from watched file - update the load time
		    update_record(M, R, Time, File, St);
		_ ->
		    St    %% not loaded from the watched file
	    end;
	error ->
	    St    %% uninteresting module
    end.


%% called whenever a file has changed and exists

changed_file(File, Time, St) ->
    %% always reread the module name from the file
    case beam_module(File) of
	{module, M} ->
	    case find_record(M, St) of
		{ok, R} ->
		    check_reload(M, File, Time, R),
		    St;
		error ->
		    %% first time we see this module: map to file
		    %% and don't try to reload
		    R = #module{file = File},
		    store_record(M, R, St)
	    end;
	error ->
	    St    %% bad file - ignore until changed again
    end.


%% check if a module needs to be reloaded, when its file has changed

check_reload(M, File, Time, R) ->
    %%erlang:display({autoload_checking, M, File, Time, R}),
    case ((R#module.file == File)
	  andalso is_loaded(M)
	  andalso (code:which(M) == File)
	  andalso is_newer(Time, R#module.time))
	of
	true ->
	    reload(M);
	false ->
	    ok
    end.


reload(M) ->
    erlang:display({autoload_loading, M}),
    code:purge(M),
    code:load_file(M).

update_record(M, R, Time, File, St) ->
    store_record(M, R#module{time = Time, file = File}, St).

find_record(M, St) ->
    dict:find(M, St#state.modules).

store_record(M, R, St) ->
    St#state{modules = dict:store(M, R, St#state.modules)}.

monitor_file(File) ->
    file_monitor:monitor_file(File, self()).

monitor_dir(Path) ->
    file_monitor:monitor_dir(Path, self()).


%% find name of module stored in a beam file

beam_module(File) ->
    case beam_lib:info(File) of
	List when is_list(List) ->
	    case lists:keysearch(module, 1, List) of
		{value, {module, M}} ->
		    {module, M};
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

%% check if the file timestamp (on DateTime format) is newer than
%% the module load-time (on Now-format)

is_newer(DateTime, LoadTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) >
	calendar:datetime_to_gregorian_seconds(
	  calendar:now_to_local_time(LoadTime)).

%% wrapper for code:is_loaded/1

is_loaded(Module) ->  
    case code:is_loaded(Module) of
	{file, _} -> true;
	false -> false
    end.
