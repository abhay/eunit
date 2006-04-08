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
%% File: eunit_text.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Text-based frontend for EUnit

%% @TODO cleanup and define the protocol for frontend processes

-module(eunit_tty).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/2]).

%% EUnit self-testing 
-include("eunit_test.hrl").

-record(state, {ref,
		succeed = 0,
		fail = 0,
		abort = false,
		indent = 0,
		cancelled = trie__new()}).

start(Reference, List) ->
    St = #state{ref = Reference},
    spawn_link(fun () -> init(List, St) end).

init(List, St0) ->
    Id = [],
    io:fwrite("=== EUnit ===\n"),
    try
	?debugmsg1("waiting for ~w begin", [Id]),
	{group, St1} = wait(Id, 'begin', St0),
	?debugmsg1("got ~w begin", [Id]),
	St2 = list(List, St1),
	?debugmsg1("waiting for ~w end", [Id]),
	{_Time, St3} = wait(Id, 'end', St2),
	?debugmsg1("got ~w end after ~w ms", [Id, _Time]),
	St3
    of
	St ->
	    ?debugmsg1("top list done: ~w", [St]),
	    Reference = St#state.ref,
	    %% @TODO FIXME: report and if possible count aborted tests!
	    receive
		{stop, Reference, ReplyTo} ->
		    io:fwrite("================================\n"
			      "  Failed: ~w.  Succeeded: ~w.\n",
			      [St#state.fail, St#state.succeed]),
		    if St#state.abort ->
			    io:fwrite("\n*** the testing was "
				      "prematurely aborted *** \n");
		       true ->
			    ok
		    end,
		    Result = if (St#state.fail > 0) or St#state.abort -> error;
				true -> ok
			     end,
		    ReplyTo ! {result, Reference, Result},
		    ok
	    end
    catch
	_Class:_Term ->
	    _Trace = erlang:get_stacktrace(),
	    ?debugmsg1("top list crashed: ~w", [{_Class,_Term,_Trace}]),
	    erlang:raise(_Class, _Term, _Trace)
    end.

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

list([E | Es], St) ->
    list(Es, entry(E, St));
list([], St) ->
    St.

entry({item, Id, Desc, Test}, St) ->
    ?debugmsg1("item: ~w", [Id]),
    {Module, Name, Line} = case Test of
			       {M, N} -> {M, N, 0};
			       _ -> Test
			   end,
    F = fun () ->
		?debugmsg1("waiting for ~w start", [Id]),
		{test, St1} = wait(Id, 'begin', St),
		indent(St#state.indent),
		io:fwrite("~s:~s~s~s...",
			  [Module, 
			   case Line of
			       0 -> "";
			       L -> io_lib:fwrite("~w:", [L])
			   end,
			   Name,
			   if Desc == "" -> "";
			      true -> io_lib:fwrite(" (~s)", [Desc])
			   end]),
		?debugmsg1("waiting for ~w end", [Id]),
		{{Result, Time}, St2} = wait(Id, 'end', St1),
		?debugmsg1("got ~w end", [Id]),
		case Result of
		    ok ->
			if Time > 0 ->
				io:fwrite("[~.3f s] ", [Time/1000]);
			   true ->
				ok
			end,
			io:fwrite("ok\n"),
			St2#state{succeed = St2#state.succeed + 1};
		    {error, Exception} ->
			io:fwrite("*failed*\n::~p\n\n", [Exception]),
			St2#state{fail = St2#state.fail + 1};
		    {skipped, Reason} ->
			skipped(Reason),
			St2#state{fail = St2#state.fail + 1}
		end
	end,
    with_abort(Id, F, fun (St) -> St end);
entry({group, Id, Desc, Es1}, St) ->
    ?debugmsg1("group: ~w", [Id]),
    F = fun () ->
		?debugmsg1("waiting for ~w begin", [Id]),
		{group, St1} = wait(Id, 'begin', St),
		I = St1#state.indent,
		St2 = if Desc == "" ->
			      St1;
			 true ->
			      indent(I),
			      io:fwrite("~s\n", [Desc]),
			      St1#state{indent = I + 1}
		      end,
		St3 = list(Es1, St2),
		?debugmsg1("waiting for ~w end", [Id]),
		{Time, St4} = wait(Id, 'end', St3),
		?debugmsg1("got ~w end", [Id]),
		if Time > 0 ->
			indent(St2#state.indent),
			io:fwrite("[group done in ~.3f s]\n", [Time/1000]);
		   true ->
			ok
		end,
		St4#state{indent = I}
	end,
    with_abort(Id, F, fun (St) -> St end).

abort(Id, St) ->
    ?debugmsg1("aborting: ~w", [Id]),
    throw({abort, Id, St}).

with_abort(Id, F, H) ->
    try F()
    catch
	{abort, Id, St} ->
	    H(St)
    end.

%% @TODO keep refining the below protocol

wait(Id, Type, St) ->
    case check_cancelled(Id, St) of
	no ->
	    receive
		{status, SomeId, {cancel, Cause}} ->
		    %% @TODO FIXME: proper handling/reporting of exit causes
		    case Cause of
			timeout ->
			    ?debugmsg1("*** ~w: timeout - test process killed by insulator\n", [SomeId]);
			{startup, _Reason} ->
			    ?debugmsg1("*** ~w: could not start test process: ~P.\n", [SomeId, _Reason, 15]);
			{exit, _Reason} ->
			    ?debugmsg1("*** ~w: test process died suddenly: ~P.\n", [SomeId, _Reason, 15]);
			{abort, _Reason} ->
			    ?debugmsg1("*** ~w: test process aborted: ~P.\n", [SomeId, _Reason, 15])
		    end,
		    wait(Id, Type, set_cancelled(SomeId, St));
		{status, Id, {progress, {Type, Data}}} ->
		    {Data, St}
%%  	      ; _Other ->
%%  		    ?debugmsg1("Unexpected message: ~w when Id = ~w.", [_Other, Id]),
%%  		    wait(Id, Type, St)
	    end;
	_ ->
	    abort(Id, St)
    end.

set_cancelled(Id, St) ->
    St#state{cancelled = trie__store(Id, St#state.cancelled)}.

check_cancelled(Id, St) ->
    trie__match(Id, St#state.cancelled).

skipped(Reason) ->
    case Reason of
	{module_not_found, M} ->
	    skip_msg("missing module: ~w", [M]);
	{no_such_function, {M,F,A}} ->
	    skip_msg("no such function: ~w:~w/~w", [M,F,A]);
	{error, Exception} ->
	    skip_msg("internal error: ~p", [Exception])
    end.

skip_msg(Str, Args) ->
    Msg = io_lib:format(Str, Args),
    io:fwrite("** did not run **\n::~s\n\n", [Msg]).

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.

%% aborted(Reason) ->
%%     case Reason of
%% 	setup_failed ->
%% 	    abort_msg("context setup failed", "", []);
%% 	cleanup_failed ->
%% 	    abort_msg("context cleanup failed", "", []);
%% 	instantiation_failed ->
%% 	    abort_msg("instantiation of subtests failed", "", []);
%% 	{bad_test, Bad} ->
%% 	    abort_msg("bad test descriptor", "~p", [Bad]);
%% 	{no_such_function, {M,F,A}} ->
%% 	    abort_msg(io_lib:format("no such function: ~w:~w/~w", [M,F,A]),
%% 			"", []);
%% 	{module_not_found, M} ->
%% 	    abort_msg("test module not found", "~p", [M]);
%% 	{generator_failed, Exception} ->
%% 	    abort_msg("test generator failed", "~p", [Exception])
%%     end.

%% abort_msg(Title, Str, Args) ->
%%     Msg = io_lib:format(Str, Args),
%%     io:fwrite("\n*** ~s ***\n::~s\n\n", [Title, Msg]).


%% a trie for remembering and checking least specific cancelled events
%% (an empty list `[]' simply represents a stored empty list, i.e., all
%% events will match, while an empty tree means that no events match)

trie__new() ->
    gb_trees:empty().

trie__store([_ | _], []) ->
    [];
trie__store([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    if Es == [] ->
		    %% overwrite any previous more specific pattern
		    gb_trees:insert(E, [], T);
	       true ->
		    gb_trees:insert(E, trie__store(Es, gb_trees:empty()), T)
	    end;
	{value, []} ->
	    T;  %% prefix already stored
	{value, T1} ->
	    gb_trees:update(E, trie__store(Es, T1), T)
    end;
trie__store([], _T) ->
    [].

trie__match([_ | _], []) ->
    prefix;
trie__match([E | Es], T) ->
    case gb_trees:lookup(E, T) of
	none ->
	    no;
	{value, []} ->
	    if Es == [] ->
		    exact;
	       true ->
		    prefix
	    end;
	{value, T1} ->
	    trie__match(Es, T1)
    end;
trie__match([], []) ->
    exact;
trie__match([], _T) ->
    no.

-ifndef(NOTEST).

trie_test_() ->
    [{"basic representation",
      [?_assert(trie__new() == gb_trees:empty()),
       ?_assert(trie__store([1], trie__new())
		== gb_trees:insert(1, [], gb_trees:empty())),
       ?_assert(trie__store([1,2], trie__new())
		== gb_trees:insert(1,
				   gb_trees:insert(2, [], gb_trees:empty()),
				   gb_trees:empty())),
       ?_assert([] == trie__store([1], [])),
       ?_assert([] == trie__store([], gb_trees:empty()))
      ]},
     {"basic storing and matching",
      [?_test(no = trie__match([], trie__new())),
       ?_test(exact = trie__match([], trie__store([], trie__new()))),
       ?_test(no = trie__match([], trie__store([1], trie__new()))),
       ?_test(exact = trie__match([1], trie__store([1], trie__new()))),
       ?_test(prefix = trie__match([1,2], trie__store([1], trie__new()))),
       ?_test(no = trie__match([1], trie__store([1,2], trie__new()))),
       ?_test(no = trie__match([1,3], trie__store([1,2], trie__new()))),
       ?_test(exact = trie__match([1,2,3,4,5],
				  trie__store([1,2,3,4,5], trie__new()))),
       ?_test(prefix = trie__match([1,2,3,4,5],
				   trie__store([1,2,3], trie__new()))),
       ?_test(no = trie__match([1,2,2,4,5],
			       trie__store([1,2,3], trie__new())))
      ]},
     {"matching with partially overlapping patterns",
      setup,
      fun () ->
	      trie__store([1,3,2], trie__store([1,2,3], trie__new()))
      end,
      fun (T) ->
	      [?_test(no = trie__match([], T)),
	       ?_test(no = trie__match([1], T)),
	       ?_test(no = trie__match([1,2], T)),
	       ?_test(no = trie__match([1,3], T)),
	       ?_test(exact = trie__match([1,2,3], T)),
	       ?_test(exact = trie__match([1,3,2], T)),
	       ?_test(no = trie__match([1,2,2], T)),
	       ?_test(no = trie__match([1,3,3], T)),
	       ?_test(prefix = trie__match([1,2,3,4], T)),
	       ?_test(prefix = trie__match([1,3,2,1], T))]
      end},
     {"matching with more general pattern overriding less general",
      setup,
      fun () -> trie__store([1], trie__store([1,2,3], trie__new())) end,
      fun (_) -> ok end,
      fun (T) ->
   	      [?_test(no = trie__match([], T)),
	       ?_test(exact = trie__match([1], T)),
 	       ?_test(prefix = trie__match([1,2], T)),
 	       ?_test(prefix = trie__match([1,2,3], T)),
 	       ?_test(prefix = trie__match([1,2,3,4], T))]
      end}
    ].

-endif.  % NOTEST
