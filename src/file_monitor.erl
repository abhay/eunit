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
%% @doc Erlang file monitoring service

%% The behaviour of this service is modelled on the open source FAM
%% daemon [http://oss.sgi.com/projects/fam/].

-module(file_monitor).

-export([start/0, start/1, start/2, stop/0, stop/1, monitor_file/2,
	 monitor_file/3, monitor_dir/2, monitor_dir/3]).

-include_lib("kernel/include/file.hrl").

-define(POLL_TIME, 5000). % default; change with option poll_time


-define(SERVER, file_monitor).

%% @TODO monitor directories, FAM-style
%%  -normal messages for the directory itself
%%  -message includes list of entries
%%     * when monitor is added for existing dir
%%     * when monitored dir changes from error to existing
%%     * but not for normal changes
%%  -

%% NOTE: paths should be absolute, but this is not checked

%% We don't change the paths, e.g. from relative to absolute, but we
%% make sure that the path is a flat string and return it to the caller.

monitor_file(Path, Pid) ->
    monitor_file(?SERVER, Path, Pid).

monitor_file(Server, Path, Pid) ->
    monitor(Server, file, filename:flatten(Path), Pid).

monitor_dir(Path, Pid) ->
    monitor_dir(?SERVER, Path, Pid).

monitor_dir(Server, Path, Pid) ->
    monitor(Server, dir, filename:flatten(Path), Pid).


monitor(Server, Type, Path, Pid) when is_pid(Pid) ->
    ServerPid = ensure_started(Server),
    ServerPid ! {monitor, self(), {Type, Path}, Pid},
    receive
	{ok, Ref} ->
	    {ok, Path, Ref}
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
    start([]).

start(Options) ->
    start(?SERVER, Options).

start(Name, Options) ->
    case whereis(Name) of
	undefined ->
	    Parent = self(),
	    Pid = spawn(fun () -> server_init(Name, Parent, Options) end),
	    receive
		{Pid, ok} -> Pid;
		{Pid, error} -> throw(no_server)
	    end;
	Pid -> Pid
    end.

-record(state, {name, time, dir, file, clients}).

server_init(Name, Parent, Options) ->
    Self = self(),
    case catch register(Name, Self) of
	true ->
	    Parent ! {Self, ok},
	    server(set_timer(init_state(Name, Options)));
	_ ->
	    Parent ! {Self, error},
	    exit(failed)
    end.

init_state(Name, Options) ->
    Time = case proplists:get_value(poll_time, Options) of
	       N when is_integer(N), N >= 100 -> N;
	       _ -> ?POLL_TIME
	   end,
    #state{name = Name,
	   time = Time,
	   dir = dict:new(),
	   file = dict:new(),
	   clients = dict:new()}.

server(St) ->
    receive
	{monitor, From, Object, Pid} when is_pid(Pid) ->
	    {Ref, St1} = new_monitor(Object, Pid, St),
	    From ! {ok, Ref},
	    server(add_client(Pid, St1));
	stop ->
	    exit(normal);
	time ->
	    server(set_timer(poll(St)));
	{'DOWN', _Ref, process, Pid, _Info} ->
	    server(purge_pid(Pid, del_client(Pid, St)));
	_ ->
	    server(St)
    end.


set_timer(St) ->
    erlang:send_after(St#state.time, self(), time),
    St.


%% client monitoring (once a client, always a client - until death)

add_client(Pid, St) ->
    case dict:is_key(Pid, St#state.clients) of
	true ->
	    St;
	false ->
	    Ref = erlang:monitor(process, Pid),
	    St#state{clients = dict:store(Pid, Ref, St#state.clients)}
    end.

del_client(Pid, St) ->
    case dict:find(Pid, St#state.clients) of
	{ok, Ref} ->
	    erlang:demonitor(Ref),
	    St#state{clients = dict:erase(Pid, St#state.clients)};
	error ->
	    St
    end.


%% no handling of directory monitoring yet

-record(monitor, {pid, reference}).

new_monitor({file, Path}, Pid, St) ->
    Ref = make_ref(),
    Monitor = #monitor{pid = Pid, reference = Ref},
    {Ref, St#state{file = add_monitor(Path, Monitor, St#state.file)}}.

%% Adding a new monitor forces an immediate poll of the file, such that
%% previous monitors only see any real change, while the new monitor
%% either gets {exists, ...} or {error, ...}.

add_monitor(Path, Monitor, Dict) ->
    {Info, Monitors} = case dict:find(Path, Dict) of
			   {ok, Data} -> poll_file(Path, Data);
			   error -> {get_file_info(Path), sets:new()}
		       end,
    event(undefined, Info, Path, sets:add_element(Monitor, sets:new())),
    dict:store(Path, {Info, sets:add_element(Monitor, Monitors)}, Dict).

%% @TODO add type to messages?

event(Info, Info, _Path, _Monitors) ->
    ok;
event(undefined, NewInfo, Path, Monitors) when not is_atom(NewInfo) ->
    cast({exists, Path, NewInfo}, Monitors);
event(_OldInfo, NewInfo, Path, Monitors) when is_atom(NewInfo) ->
    cast({error, Path, NewInfo}, Monitors);
event(_OldInfo, NewInfo, Path, Monitors) ->
    cast({changed, Path, NewInfo}, Monitors).


poll(St) ->
    St#state{file = dict:map(fun poll_file/2, St#state.file)}.

poll_file(Path, {OldInfo, Monitors}) ->
    NewInfo = get_file_info(Path),
    event(OldInfo, NewInfo, Path, Monitors),
    {NewInfo, Monitors}.


%% We clear some fields of the file_info so that we only trigger on real
%% changes; see the //kernel/file.erl manual and file.hrl for details.

get_file_info(Path) ->
    case file:read_file_info(Path) of
	{ok, Info} ->
	    Info#file_info{access = undefined,
			   atime  = undefined};
	{error, Error} ->
	    Error  % posix error code as atom
    end.


cast(Msg, Ms) ->
    sets:fold(fun (#monitor{pid = Pid, reference = Ref}, Msg) ->
		      erlang:display({file_monitor, Ref, Msg}),
		      Pid ! {file_monitor, Ref, Msg},
		      Msg  % note that this is a fold, not a map
	      end,
	      Msg, Ms).


purge_pid(Pid, St) ->
    Files = dict:map(fun (_Path, {Info, Monitors}) ->
			     {Info, purge_monitor_pid(Pid, Monitors)}
		     end,
		     St#state.file),
    St#state{file = purge_empty_sets(Files)}.

purge_monitor_pid(Pid, Monitors) ->
    sets:filter(fun (#monitor{pid = P}) when P == Pid -> false;
		    (_) -> true
		end,
		Monitors).

purge_empty_sets(Dict) ->
    dict:filter(fun (_Path, {_Info, Monitors}) ->
			sets:size(Monitors) > 0
		end, Dict).
