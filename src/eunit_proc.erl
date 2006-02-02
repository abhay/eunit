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
%% File: eunit_proc.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test runner process tree functions

-module(eunit_proc).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/4]).


-record(procstate, {ref, super, parent, order}).


start(Tests, Reference, Super, Order) ->
    spawn_tester(Tests, init_procstate(Reference, Super, Order)).

init_procstate(Reference, Super, Order) ->
    #procstate{ref = Reference, super = Super, order = Order}.

message_super(Msg, St) ->
    St#procstate.super ! Msg.


%% ---------------------------------------------------------------------
%% Process tree primitives

%% A "task" consists of an insulator process and a child process which
%% handles the actual work. When the child terminates, the insulator
%% process sends {self(), ExitReason} to the process which started the
%% task (the "parent"). The child process is given a State record which
%% contains the process id:s of the parent and the supervisor; however,
%% the insulator pid is not known to the child.

%% @spec ((#procstate{}) -> () -> term(), #procstate{}) -> pid()

start_task(Fun, St0) ->
    Parent = self(),
    St = St0#procstate{parent = Parent},
    spawn_link(fun () -> insulator_process(Fun(St), St) end).

%% Simple, failure-proof insulator process
%% (This is cleaner than temporarily setting up the caller to trap
%% signals, and does not affect the caller's mailbox or other state.)

%% @spec (Fun::() -> term(), St::#procstate{}) -> ok

%% @TODO track state of child process by receiving messages
%% @TODO adjustable timeouts; no timeout if child itself is in waiting state

insulator_process(Fun, St) ->
    process_flag(trap_exit, true),
    Child = spawn_link(fun () -> child_process(Fun, St) end),
    Parent = St#procstate.parent,
    receive
	{'EXIT', Child, Reason} ->
	    task_reply(Reason, St),
	    ok;
	{'EXIT', Parent, _} ->
	    terminate_insulator(parent_died, Child)
    after 5000 ->
	    terminate_insulator(timed_out, Child)
    end.

terminate_insulator(Reason, Child) ->
    exit(Child, kill),
    exit(Reason).

%% @spec (() -> term(), #procstate{}) -> ok

child_process(Fun, St) ->
    try Fun() of
	_ -> ok
    catch
	{abort, Reason} ->
	    St#procstate.super ! {abort, Reason},
	    exit(aborted)
    end.

task_reply(ExitReason, St) ->
    St#procstate.parent ! {St#procstate.ref, self(), ExitReason}.
    
%% Typically, the process that executes this code is not trapping
%% signals, but it might be - it is outside of our control. That is why
%% the insulator process of a task must be guaranteed to always send a
%% reply before it terminates (unless it is forcibly killed).
%%
%% The unique reference guarantees that we don't extract any message
%% from the mailbox unless it belongs to the test framework. When the
%% wait-loop terminates, no such message should remain in the mailbox.

wait_for_task(Pid, St) ->
    wait_for_tasks(sets:from_list([Pid]), St).

wait_for_tasks(PidSet, St) ->
    case sets:size(PidSet) of
	0 ->
	    ok;
	_ ->
	    Reference = St#procstate.ref,
	    receive
		{Reference, Pid, Reason} ->
 		    if Reason /= normal ->
 			    exit({child_died, Pid, Reason});
		       true ->
			    %% (if Pid is not in the set, the below has
			    %% no effect; however, that cannot happen)
 			    Rest = sets:del_element(Pid, PidSet),
 			    wait_for_tasks(Rest, St)
		    end
	    end
    end.

%% @throws abortException()
%% @type abortException() = {abort, Reason::term()}

abort_task(Reason) ->
    throw({abort, Reason}).


%% ---------------------------------------------------------------------
%% Separate testing process

spawn_tester(T, St0) ->
    Fun = fun (St) ->
		  fun () -> group(T, St) end
	  end,
    start_task(Fun, St0).

%% @throws abortException()

group(T, St) ->
    I = eunit_data:iter_init(T),
    case St#procstate.order of
	true -> group_inorder(I, St);
	false -> group_inparallel(I, St)
    end.

%% @throws abortException()

group_inorder(I, St) ->
    case get_next_test(I) of
	{T, I1} ->
	    handle_item(T, St),
	    group_inorder(I1, St);
	none ->
	    ok
    end.

%% @throws abortException()

group_inparallel(I, St) ->
    group_inparallel(I, St, sets:new()).

group_inparallel(I, St, Children) ->
    case get_next_test(I) of
	{T, I1} ->
	    Child = spawn_item(T, St),
	    group_inparallel(I1, St, sets:add_element(Child, Children));
	none ->
	    wait_for_tasks(Children, St),
	    ok
    end.

%% @throws abortException()

spawn_item(T, St0) ->
    Fun = fun (St) ->
		  fun () -> handle_item(T, St) end
	  end,
    start_task(Fun, St0).

get_next_test(I) ->
    try eunit_data:iter_next(I)
    catch
	R = {bad_test, _Bad} ->
	    abort_task(R);
	R = {no_such_function, _MFA} ->
	    abort_task(R);
	R = {module_not_found, _M} ->
	    abort_task(R);
	R = {generator_failed, _Exception} ->
	    abort_task(R)
    end.

%% @throws abortException()

handle_item(T, St) ->
    case T of
	#test{} -> handle_test(T, St);
	#group{} -> handle_group(T, St);
	#context{} -> handle_context(T, St) 
    end.

handle_test(T, St) ->
    message_super({test, start, {T#test.module, T#test.line,
				 T#test.name, T#test.desc}},
		  St),
    Status = try run_test(T) of
		 {ok, _Value} ->
		     %% just throw away the return value
		     ok;
		 {error, Exception} ->
		     {error, Exception}
	     catch
		 R = {module_not_found, _M} ->
		     {skipped, R};
		 R = {no_such_function, _MFA} ->
		     {skipped, R}
	     end,
    message_super({test, done, Status}, St),
    ok.

handle_group(T, St0) ->
    St = set_group_order(T, St0),
    T1 = T#group.tests,
    message_super({group, enter, T#group.desc}, St),
    case T#group.spawn of
	true ->
	    Child = spawn_tester(T1, St),
	    wait_for_task(Child, St);
	_ ->
	    group(T1, St)
    end,
    message_super({group, leave, T#group.desc}, St),
    ok.

set_group_order(#group{order = undefined}, St) ->
    St;
set_group_order(#group{order = Order}, St) ->
    St#procstate{order = Order}.

%% @throws abortException()

handle_context(T, St) ->
    try
	enter_context(T, fun (T) -> group(T, St) end)
    catch
	R = setup_failed ->
	    abort_task(R);
	R = cleanup_failed ->
	    abort_task(R);
	R = instantiation_failed ->
	    abort_task(R)
    end.

%% @spec (#test{}) -> {ok, Value} | {error, exception()}
%% @throws eunit_test:wrapperError()

run_test(#test{f = F}) ->
    eunit_test:run_testfun(F).


%% @throws instantiation_failed

browse_context(#context{instantiate = I}, F) ->
    %% Browse: dummy setup/cleanup and a wrapper for the instantiator
    S = fun () -> ok end,
    C = fun (_) -> ok end,
    I1 = fun (_) ->
		try eunit_lib:browse_fun(I) of
		    {ok, _, T} ->
			T;
		    error ->
			throw(instantiation_failed)
		catch
		    _:_ ->
			throw(instantiation_failed)
		end
	 end,
    enter_context(S, C, I1, F).

%% @spec (Tests::#context{}, Callback::(any()) -> any()) -> any()
%% @throws setup_failed | instantiation_failed | cleanup_failed

enter_context(#context{setup = S, cleanup = C, instantiate = I}, F) ->
    enter_context(S, C, I, F).

enter_context(Setup, Cleanup, Instantiate, Callback) ->
    try Setup() of
	R ->
	    try Instantiate(R) of
		T ->
		    try Callback(T)  %% call back to client code
		    after
			%% Always run cleanup; client may be an idiot
			try Cleanup(R)
			catch
			    _:_ -> throw(cleanup_failed)
			end
		    end
	    catch
		_:_ ->
		    throw(instantiation_failed)
	    end
    catch
	_:_ ->
	    throw(setup_failed)
    end.
