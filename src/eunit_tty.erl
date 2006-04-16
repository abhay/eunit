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

%% @TODO less verbose mode by default

-module(eunit_tty).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/1]).

%% EUnit self-testing 
-include("eunit_test.hrl").

-record(state, {succeed = 0,
		fail = 0,
		abort = false,
		indent = 0,
		cancelled = trie__new(),
		results = dict:new()}).

start(List) ->
    St = #state{},
    Id = [],
    spawn(fun () -> init(Id, List, St) end).

init(Id, List, St0) ->
    io:fwrite("=== EUnit ===\n"),
    try
	top(Id, List, St0)
    of
	St -> done(St)
    catch
	_Class:_Term ->
	    _Trace = erlang:get_stacktrace(),
	    ?debugmsg1("top list crashed: ~w", [{_Class,_Term,_Trace}]),
	    erlang:raise(_Class, _Term, _Trace)
    end.

top(Id, List, St0) ->
    ?debugmsg1("waiting for ~w begin", [Id]),
    {{ok, group}, St1} = wait(Id, 'begin', St0),
    ?debugmsg1("got ~w begin", [Id]),
    St2 = list(List, St1),
    ?debugmsg1("waiting for ~w end", [Id]),
    {{ok, {ok, _Time}}, St3} = wait(Id, 'end', St2),
    ?debugmsg1("got ~w end after ~w ms", [Id, _Time]),
    St3.

done(St) ->
    ?debugmsg1("top list done: ~w", [St]),
    %% @TODO report and if possible count aborted tests!
    receive
	{stop, ReplyTo, Reference} ->
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
    Where = case Test of
		{M, N} -> {M, N, 0};
		_ -> Test
	    end,
    test_begin(Id, Desc, Where, St);
entry({group, Id, Desc, Es}, St) ->
    ?debugmsg1("group: ~w", [Id]),
    group_begin(Id, Desc, Es, St).

test_begin(Id, Desc, {Module, Name, Line}, St) ->
    ?debugmsg1("waiting for ~w start", [Id]),
    case wait(Id, 'begin', St) of
	{{abort, Reason}, St1} ->
	    indent(St1#state.indent),
	    io:fwrite("*aborted* (~w)::~p\n", [Id, Reason]),
	    %% TODO: better handling of indentation and so forth here
%% 	    if Type == 'end' ->
%% 		    io:fwrite("*aborted*::~p\n", [Reason]);
%% 	       true ->
%% 		    ok
%% 	    end,
	    St1;
	{{ok, test}, St1} ->
	    indent(St1#state.indent),
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
	    test_end(Id, St1)
    end.

test_end(Id, St) ->
    ?debugmsg1("waiting for ~w end", [Id]),
    case wait(Id, 'end', St) of
	{{abort, _}, St1} ->
	    St1;
	{{ok, {Result, Time}}, St1} ->
	    ?debugmsg1("got ~w end", [Id]),
	    case Result of
		ok ->
		    if Time > 0 ->
			    io:fwrite("[~.3f s] ", [Time/1000]);
		       true ->
			    ok
		    end,
		    io:fwrite("ok\n"),
		    St1#state{succeed = St1#state.succeed + 1};
		{error, Exception} ->
		    io:fwrite("*failed*\n::~p\n\n", [Exception]),
		    St1#state{fail = St1#state.fail + 1};
		{skipped, SkipReason} ->
		    io:fwrite("*did not run*\n::~s\n\n",
			      [format_skip_reason(SkipReason)]),
		    St1#state{fail = St1#state.fail + 1}
	    end
    end.

group_begin(Id, Desc, Es, St) ->
    ?debugmsg1("waiting for ~w begin", [Id]),
    case wait(Id, 'begin', St) of
	{{abort, _}, St1} ->
	    St1;
	{{ok, group}, St1} ->
	    I = St1#state.indent,
	    St2 = if Desc == "" ->
			  St1;
		     true ->
			  indent(I),
			  io:fwrite("~s\n", [Desc]),
			  St1#state{indent = I + 1}
		  end,
	    group_end(Id, I, list(Es, St2))
    end.

group_end(Id, I, St) ->
    ?debugmsg1("waiting for ~w end", [Id]),
    case wait(Id, 'end', St) of
	{{abort, _}, St1} ->
	    St1;
	{{ok, {ok, Time}}, St1} ->
	    ?debugmsg1("got ~w end", [Id]),
	    if Time > 0 ->
		    indent(St1#state.indent),
		    io:fwrite("[group done in ~.3f s]\n", [Time/1000]);
	       true ->
		    ok
	    end,
	    St1#state{indent = I}
    end.

wait(Id, Type, St) ->
    case check_cancelled(Id, St) of
	no ->
	    receive
		{status, SomeId, {cancel, Cause}} ->
		    %% @TODO proper reporting of exit causes (not here)
		    case Cause of
			timeout ->
			    ?debugmsg1("*** ~w: timeout - test process killed by insulator\n", [SomeId]);
			{startup, _Reason} ->
			    ?debugmsg1("*** ~w: could not start test process: ~P.\n", [SomeId, _Reason, 15]);
			{blame, _SubId} ->
			    ?debugmsg1("*** ~w: cancelled because of: ~w.\n", [SomeId, _SubId]);
			{exit, _Reason} ->
			    ?debugmsg1("*** ~w: test process died suddenly: ~P.\n", [SomeId, _Reason, 15]);
			{abort, _Reason} ->
			    ?debugmsg1("*** ~w: test process aborted: ~P.\n", [SomeId, _Reason, 15])
		    end,
		    wait(Id, Type, set_cancelled(SomeId, Cause, St));
		{status, Id, {progress, {Type, Data}}} ->
		    {{ok, Data}, St}
%%  	      ; _Other ->
%%  		    ?debugmsg1("Unexpected message: ~w when Id = ~w.", [_Other, Id]),
%%  		    wait(Id, Type, St)
	    end;
	{yes, Reason} ->
	    {{abort, Reason}, St}
    end.

set_cancelled(Id, Cause, St) ->
    St1 = St#state{cancelled = trie__store(Id, St#state.cancelled)},
    set_result(Id, {cancelled, Cause}, St1).

check_cancelled(Id, St) ->
    case trie__match(Id, St#state.cancelled) of
	no -> no;
	_ -> {yes, case dict:find(Id, St#state.results) of
		       {ok, Result} -> Result;
		       error -> undefined
		   end}
    end.

set_result(Id, Result, St) ->
    St#state{results = dict:store(Id, Result, St#state.results)}.

format_skip_reason(Reason) ->
    case Reason of
	{module_not_found, M} ->
	    io_lib:format("missing module: ~w", [M]);
	{no_such_function, {M,F,A}} ->
	    io_lib:format("no such function: ~w:~w/~w", [M,F,A])
    end.

indent(N) when is_integer(N), N >= 1 ->
    io:put_chars(lists:duplicate(N * 2, $\s));
indent(_) ->
    ok.

%% @TODO put these messages back in use
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

-ifdef(TEST).

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

-endif.  % TEST
