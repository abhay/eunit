%%%-------------------------------------------------------------------
%% File: eunit_internal.hrl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @doc 

%%-define(NOTEST, true).
-define(NDEBUG, true).


-define(DEFAULT_TEST_SUFFIX, "_test").
-define(DEFAULT_GENERATOR_SUFFIX, "_test_").
-define(DEFAULT_INSULATOR_TIMEOUT, 5000).


%% ---------------------------------------------------------------------
%% Protocol records

-ifndef(NDEBUG).
-define(debugmsg(S),io:fwrite("\n* ~s: ~s\n", [?MODULE,S])).
-define(debugmsg1(S,As),io:fwrite("\n* ~s: " ++ S ++ "\n", [?MODULE] ++ As)).
-else.
-define(debugmsg(S),ok).
-define(debugmsg1(S,As),ok).
-endif.

-record(test, {f = undefined,
	       desc = undefined,
	       module = undefined,
	       name = undefined,
	       line = 0
	      }).

-record(context, {setup = undefined,
		  cleanup = undefined,
		  instantiate = undefined}).

-record(group, {desc = undefined,	% optional description
		spawn = undefined,	% run group in new process
		order = undefined,	% run in order or in parallel
		tests = undefined}).
