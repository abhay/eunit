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
%% @copyright 2004-2006 Micka�l R�mond, Richard Carlsson
%% @author Micka�l R�mond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @author Richard Carlsson <richardc@it.uu.se>
%%   [http://user.it.uu.se/~richardc/]
%% @version {@vsn}, {@date} {@time}
%% @doc This module is the normal EUnit user interface.

-module(eunit).

%% @TODO make a logger process to capture all events unconditionally

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([start/0, start/1, stop/0, stop/1, test/1, test/2, test/3,
	 list/1, submit/1, submit/2, submit/3, watch/1, watch/2,
	 watch/3, watch_path/1, watch_path/2, watch_path/3,
	 watch_regexp/1, watch_regexp/2, watch_regexp/3, watch_app/1,
	 watch_app/2, watch_app/3]).

-export([testp/1]). %% for development testing, not official


-ifdef(TEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    [eunit_proc, eunit_test, eunit_lib, eunit_data, eunit_tty].
-endif.


%% EUnit entry points

start() ->
    start(?SERVER).

start(Server) ->
    eunit_server:start(Server).

stop() ->
    stop(?SERVER).

stop(Server) ->
    eunit_server:stop(Server).

watch(Target) ->
    watch(Target, []).

watch(Target, Options) ->
    watch(?SERVER, Target, Options).

watch(Server, Target, Options) ->
    eunit_server:watch(Server, Target, Options).

watch_path(Target) ->
    watch_path(Target, []).

watch_path(Target, Options) ->
    watch_path(?SERVER, Target, Options).

watch_path(Server, Target, Options) ->
    eunit_server:watch_path(Server, Target, Options).

watch_regexp(Target) ->
    watch_regexp(Target, []).

watch_regexp(Target, Options) ->
    watch_regexp(?SERVER, Target, Options).

watch_regexp(Server, Target, Options) ->
    eunit_server:watch_regexp(Server, Target, Options).

watch_app(Name) ->
    watch_app(Name, []).

watch_app(Name, Options) ->
    watch_app(?SERVER, Name, Options).

watch_app(Server, Name, Options) ->
    case code:lib_dir(Name) of
	Path when is_list(Path) ->
	    watch_path(Server, filename:join(Path, "ebin"), Options);
	_ ->
	    error
    end.

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
    try eunit_data:list(T) of
	List ->
	    Front = eunit_tty:start(List, Options),
	    Serial = eunit_serial:start(List, [Front]),
	    case eunit_server:start_test(Server, Serial, T, Options) of
		{ok, Reference} -> test_run(Reference, Front);
		{error, R} -> {error, R}
	    end
    catch
	{error, R} -> {error, R}
    end.

test_run(Reference, Front) ->
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



