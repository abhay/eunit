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
%% $Id: eunit.erl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% @copyright 2004-2006 Mickaël Rémond, Richard Carlsson
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@it.uu.se>
%%   [http://user.it.uu.se/~richardc/]
%% @version {@vsn}, {@date} {@time}
%% @doc This module is the normal EUnit user interface.

-module(eunit).

%% @TODO make a logger process to capture all events unconditionally

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([start/0, stop/0, test/1, test/2, test/3, list/1, submit/1,
	 submit/2, submit/3, watch/1]).

-export([testp/1]). %% for development testing, not official

%% Self-testing
-include("eunit_test.hrl").


-ifdef(TEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    [eunit_proc, eunit_test, eunit_lib, eunit_data, eunit_tty].
-endif.


%% EUnit entry points

start() ->
    eunit_server:start(?SERVER).

stop() ->
    eunit_server:stop(?SERVER).

watch(Target) ->
    eunit_server:watch(?SERVER, Target).

list(T) ->
    try eunit_data:list(T)
    catch
	{error, R} -> {error, R}
    end.

test(T) ->
    test(T, [{order, inorder}]).

testp(T) ->
    test(T, [{order, inparallel}]).

test(T, Options) ->
    test(?SERVER, T, Options).

test(Server, T, Options) ->
    List = eunit_data:list(T),
    Front = eunit_tty:start(List, Options),
    Serial = eunit_serial:start(List, [Front]),
    case eunit_server:start_test(Server, Serial, T, Options) of
	{ok, Reference} ->
	    receive
		{done, Reference} ->
		    Front ! {stop, self(), Reference},
		    receive 
			{result, Reference, Result} ->
			    Result
		    end
	    end;
	{error, R} -> {error, R}
    end.

%% TODO: functions that run tests on a given node, not a given server
%% TODO: maybe some functions could check for a globally registered server?
%% TODO: some synchronous but completely quiet interface function

submit(T) ->
    submit(T, []).

submit(T, Options) ->
    submit(?SERVER, T, Options).

submit(Server, T, Options) ->
    Dummy = spawn(fun devnull/0),
    eunit_server:start_test(Server, Dummy, T, Options).

%% TODO: make a proper logger for asynchronous execution with submit/3

devnull() ->
    receive _ -> devnull() end.



