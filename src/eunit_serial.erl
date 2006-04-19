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
%% File: eunit_serial.erl
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
