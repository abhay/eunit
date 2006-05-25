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
	 submit/2, submit/3, watch/1, watch_path/1, watch_regexp/1,
	 watch_app/1]).

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

watch_path(Target) ->
    eunit_server:watch_path(?SERVER, Target).

watch_regexp(Target) ->
    eunit_server:watch_regexp(?SERVER, Target).

watch_app(Name) ->
    eunit_server:watch_app(?SERVER, code:lib_dir(Name) ++ "/ebin").

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
		{start, Reference} ->
		    Front ! {start, Reference}
	    end,
	    receive
		{done, Reference} ->
		    Front ! {stop, Reference, self()},
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



