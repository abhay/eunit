%% ---------------------------------------------------------------------
%% File: eunit_tests.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richard@it.uu.se>
%% @copyright 2007 Richard Carlsson
%% @doc 

-module(eunit_tests).

-export([]).

-include("eunit.hrl").

-ifdef(TEST).
%% Cause all the other modules to be tested as well as this one.
full_test_() ->
    [eunit_autoexport,
     eunit_striptests,
     eunit_server,
     eunit_proc,
     eunit_serial,
     eunit_test,
     eunit_lib,
     eunit_data,
     eunit_tty,
     code_monitor,
     file_monitor,
     autoload].
-endif.
