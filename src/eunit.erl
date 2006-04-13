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

%% Self-testing
-include("eunit_test.hrl").


-ifndef(NOTEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    [eunit_proc, eunit_test, eunit_lib, eunit_data, eunit_tty].
-endif.


%% New EUnit entry points

list(T) ->
    eunit_data:list(T).

test(T) ->
    test(T, [{order, inorder}]).

testp(T) ->
    test(T, [{order, inparallel}]).

test(T, Options) ->
    %% The default is to run tests in order unless otherwise specified
    Order = proplists:get_value(order, Options, inorder),
    Reference = make_ref(),
    Super = eunit_tty:start(Reference, list(T)),
    Root = eunit_proc:start(T, Reference, Super, Order),
    wait(Reference, Root, Super).

%% @TODO better system for setting up and waiting for tests and interface

wait(Reference, Root, Front) ->
    receive
	{done, Reference, Root} ->
	    ?debugmsg("*** received done message from Root process\n"),
	    done(Reference, Front)
%%    ; _Other ->
%% 	    ?debugmsg1("Unexpected message: ~w.", [_Other]),
%% 	    wait(Reference, Root, Front)
    end.

done(Reference, Front) ->
    Front ! {stop, Reference, self()},
    receive 
	{result, Reference, Result} ->
	    Result
    end.
