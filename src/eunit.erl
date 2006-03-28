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

-include("eunit.hrl").
-include("eunit_internal.hrl").


-export([test/1, test/2, list/1]).
-export([testp/1]). %% for development testing, not official

%% Old EUnit entry point. Don't use.
-export([run/2]).

%% Aaegis support. (Old entry point - don't use unless you must.)
-export([erlfilename/1]).

%% Self-testing
-include("eunit_test.hrl").


-ifndef(NOTEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    [eunit_proc, eunit_test, eunit_lib, eunit_data, eunit_old].
-endif.


%% Old EUnit entry points. Avoid!

run(Modules, Pattern) ->
    eunit_old:run(Modules, Pattern).

erlfilename(Erlfile) ->
    eunit_old:erlfilename(Erlfile).


%% New EUnit entry points

list(T) ->
    eunit_data:list(T).

test(T) ->
    test(T, [{order, true}]).

testp(T) ->
    test(T, [{order, false}]).

test(T, Options) ->
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_bool(order, Options),
    Super = self(),
    Reference = make_ref(),
    Root = eunit_proc:start(T, Reference, Super, Order),
    Front = eunit_text:start(T, Reference),
    wait(Reference, Root, Front).


%% @TODO fix so that we don't have to forward messages to the frontend

wait(Reference, Root, Front) ->
    receive
	{Reference, Root} ->	
	    done(Reference, Front);
	{died, Pid, Reason} ->
	    %% FIXME: do something better here; identify the failed test/group
	    io:fwrite("*** test process ~w died suddenly: ~P.\n", [Pid, Reason, 15]),
	    wait(Reference, Root, Front);
	{killed, Pid, Reason} ->
	    %% FIXME: do something better here; identify the failed test/group
	    io:fwrite("*** test process ~w killed by insulator: ~P.\n", [Pid, Reason, 15]),
	    wait(Reference, Root, Front);
	Msg ->
	    Front ! Msg,
	    wait(Reference, Root, Front)
    end.

done(Reference, Front) ->
    Front ! {Reference, stop, self()},
    receive 
	{Reference, result, Result} ->
	    Result
    end.
