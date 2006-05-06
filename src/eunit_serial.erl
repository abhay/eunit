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
%% @doc Event serializing process which works as an adapter and
%% multiplexer for "supervisor" processes

-module(eunit_serial).

-export([start/2]).

%% Notes:
%% * A cancelling event may arrive at any time, and may concern items we
%% are not yet expecting (if tests are executed in parallel), or may
%% concern not only the current item but possibly a group ancestor of
%% the current item (as in the case of a group timeout).
%% 
%% * It is not possible to use selective receive to extract only those
%% cancelling messages that affect the current item and its parents;
%% basically, because we cannot have a dynamically computed prefix as a
%% pattern in a receive. Hence, we must extract each cancelling event as
%% it arrives and keep track of them separately.
%% 
%% * Before we wait for a new item, we must check whether it (and thus
%% also all its subitems, if any) is already cancelled.
%% 
%% * When a new cancelling event arrives, we must either store it for
%% future use, and/or cancel the current item and possibly one or more
%% of its parent groups.

-record(state, {listeners,
		cancelled = eunit_lib:trie_new(),
		messages = dict:new()}).

start(List, Pids) ->
    spawn(fun () -> serializer(List, Pids) end).

serializer(List, Pids) ->
    St = #state{listeners = sets:from_list(Pids),
		cancelled = eunit_lib:trie_new(),
		messages = dict:new()},
    entry(group, [], List, St),
    exit(normal).

entry(Type, Id, Body, St) ->
    case wait(Id, 'begin', St) of
	{{cancel, undefined}, St1} ->
	    cast({status, Id, {cancel, undefined}}, St1);
	{{cancel, Msg}, St1} ->
	    cast(Msg, St1);
	{{ok, Msg}, St1} ->
	    cast(Msg, St1),
	    St2 = if Type == group -> tests(Body, St1);
		     true -> St1
		  end,
	    case wait(Id, 'end', St2) of
		{{cancel, undefined}, St3} ->
		    cast({status, Id, {cancel, undefined}}, St3);
		{{_, Msg1}, St3} ->
		    cast(Msg1, St3)
	    end
    end.

tests([{Type, Id, _Desc, Body} | Es], St) ->
    tests(Es, entry(Type, Id, Body, St));
tests([], St) ->
    St.

cast(M, St) ->
    sets:fold(fun (L, M) -> L ! M end, M, St#state.listeners),
    St.

wait(Id, Type, St) ->
    case check_cancelled(Id, St) of
	no ->
	    receive
		{status, SomeId, {cancel, _Cause}} = Msg ->
		    wait(Id, Type, set_cancelled(SomeId, Msg, St));
		{status, Id, {progress, Type, _Data}} = Msg ->
		    {{ok, Msg}, St}
	    end;
	{yes, Msg} ->
	    {{cancel, Msg}, forget(Id, St)}
    end.

set_cancelled(Id, Msg, St0) ->
    St = remember(Id, Msg, St0),
    St#state{cancelled = eunit_lib:trie_store(Id, St0#state.cancelled)}.

check_cancelled(Id, St) ->
    case eunit_lib:trie_match(Id, St#state.cancelled) of
	no -> no;
	_ -> {yes, recall(Id, St)}
    end.

remember(Id, Msg, St) ->
    St#state{messages = dict:store(Id, Msg, St#state.messages)}.

forget(Id, St) ->
    St#state{messages = dict:store(Id, undefined, St#state.messages)}.

recall(Id, St) ->
    case dict:find(Id, St#state.messages) of
	{ok, Msg} -> Msg;
	error -> undefined
    end.
