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
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Test runner process tree functions

-module(eunit_proc).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([start/4]).


-record(procstate, {ref, id, super, insulator, parent, order}).


%% spawns test process and returns the process Pid; sends {Reference,
%% Pid} to caller when finished

start(Tests, Reference, Super, Order) ->
    spawn_tester(Tests, init_procstate(Reference, Super, Order)).

init_procstate(Reference, Super, Order) ->
    #procstate{ref = Reference, id = [], super = Super, order = Order}.


%% ---------------------------------------------------------------------
%% Process tree primitives

%% A "task" consists of an insulator process and a child process which
%% handles the actual work. When the child terminates, the insulator
%% process sends {Reference, self()} to the process which started the
%% task (the "parent"). The child process is given a State record which
%% contains the process id:s of the parent, the insulator, and the
%% supervisor.

%% @spec ((#procstate{}) -> () -> term(), #procstate{}) -> pid()

start_task(Fun, St0) ->
    St = St0#procstate{parent = self()},
    %% (note: the link here is mainly to propagate signals *downwards*,
    %% so that the insulator can detect if the process that started the
    %% task dies before the task is done)
    spawn_link(fun () -> insulator_process(Fun, St) end).

%% Simple, failure-proof insulator process
%% (This is cleaner than temporarily setting up the caller to trap
%% signals, and does not affect the caller's mailbox or other state.)

%% @spec (Fun::() -> term(), St::#procstate{}) -> ok

insulator_process(Fun, St0) ->
    process_flag(trap_exit, true),
    St = St0#procstate{insulator = self()},
    Child = spawn_link(fun () -> child_process(Fun(St), St) end),
    Parent = St#procstate.parent,
    insulator_wait(Child, Parent, St).

%% Normally, child processes exit with the reason 'normal' even if the
%% executed tests failed (by throwing exceptions). Child processes can
%% terminate abnormally by 1) a "syntax error" in the test descriptors;
%% 2) failure in a setup, cleanup or initialize; 3) an internal error in
%% the EUnit test runner framework; or 4) receiving a non-trapped error
%% signal as a consequence of running test code. Since alt. 4) implies
%% that the test neither reported success nor failure, it can never be
%% considered "proper" behaviour of a test. Abnormal termination is
%% reported to the supervisor process but otherwise does not affect the
%% insulator compared to normal termination.

insulator_wait(Child, Parent, St) ->
    receive
	{'EXIT', Child, normal} ->
	    terminate_insulator(St);
	{'EXIT', Child, Reason} ->
	    St#procstate.super ! {died, Child, Reason},
	    terminate_insulator(St);
	{'EXIT', Parent, _} ->
	    kill_task(Child, parent_died, St);
	{Child, {timeout, Id}} ->
	    kill_task(Child, {timed_out, Id}, St)
    end.

%% Unlinking before exit avoids polluting the parent process with exit
%% signals from the insulator. The child process is already dead here.

terminate_insulator(St) ->
    %% messaging/unlinking is ok even if the parent is already dead
    Parent = St#procstate.parent,
    Parent ! {St#procstate.ref, self()},
    unlink(Parent),
    exit(normal).

kill_task(Child, Reason, St) ->
    exit(Child, kill),
    St#procstate.super ! {killed, Child, Reason},
    terminate_insulator(St).

set_timeout(Time, St) ->
    erlang:send_after(Time, St#procstate.insulator,
		      {self(), {timeout, St#procstate.id}}).

clear_timeout(Ref) ->
    erlang:cancel_timer(Ref).

with_timeout(undefined, Default, F, St) ->
    with_timeout(Default, F, St);
with_timeout(Time, _Default, F, St) ->
    with_timeout(Time, F, St).

with_timeout(infinity, F, _St) ->
    F();
with_timeout(Time, F, St) when is_integer(Time), Time >= 0 ->
    Ref = set_timeout(Time, St),
    try F()
    after
	clear_timeout(Ref)
    end.

%% The normal behaviour of a child process is to trap exit signals. This
%% makes it easier to write tests that spawn off separate (linked)
%% processes and test whether they terminate as expected. The testing
%% framework is not dependent on this, however, so the test code is
%% allowed to disable signal trapping as it pleases.

%% @spec (() -> term(), #procstate{}) -> ok

child_process(Fun, St) ->
    process_flag(trap_exit, true),
    try Fun() of
	_ -> ok
    catch
	{abort, Reason} ->
	    St#procstate.super ! {abort, Reason},
	    exit(aborted)
    end.

%% @throws abortException()
%% @type abortException() = {abort, Reason::term()}

abort_task(Reason) ->
    throw({abort, Reason}).

%% Typically, the process that executes this code is trapping signals,
%% but it might not be - it is outside of our control, since test code
%% could turn off trapping. That is why the insulator process of a task
%% must be guaranteed to always send a reply before it terminates.
%%
%% The unique reference guarantees that we don't extract any message
%% from the mailbox unless it belongs to the test framework (and not to
%% the running tests). When the wait-loop terminates, no such message
%% should remain in the mailbox.

wait_for_task(Pid, St) ->
    wait_for_tasks(sets:from_list([Pid]), St).

wait_for_tasks(PidSet, St) ->
    case sets:size(PidSet) of
	0 ->
	    ok;
	_ ->
	    %% (note that when we receive this message for some task, we
	    %% are guaranteed that the insulator process of the task has
	    %% already informed the supervisor about any anomalies)
	    Reference = St#procstate.ref,
	    receive
		{Reference, Pid} ->
		    %% (if Pid is not in the set, del_element has no
		    %% effect, so this is always safe)
		    Rest = sets:del_element(Pid, PidSet),
		    wait_for_tasks(Rest, St)
	    end
    end.


%% ---------------------------------------------------------------------
%% Separate testing process

spawn_tester(T, St0) ->
    Fun = fun (St) ->
		  fun () -> tests(T, St) end
	  end,
    start_task(Fun, St0).

%% @throws abortException()

tests(T, St) ->
    I = eunit_data:iter_init(T, St#procstate.id),
    case St#procstate.order of
	true -> tests_inorder(I, St);
	false -> tests_inparallel(I, St)
    end.

set_id(I, St) ->
    St#procstate{id = eunit_data:iter_id(I)}.

%% @throws abortException()

tests_inorder(I, St) ->
    case get_next_item(I) of
	{T, I1} ->
	    handle_item(T, set_id(I1, St)),
	    tests_inorder(I1, St);
	none ->
	    ok
    end.

%% @throws abortException()

tests_inparallel(I, St) ->
    tests_inparallel(I, St, sets:new()).

tests_inparallel(I, St, Children) ->
    case get_next_item(I) of
	{T, I1} ->
	    Child = spawn_item(T, set_id(I1, St)),
	    tests_inparallel(I1, St, sets:add_element(Child, Children));
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

get_next_item(I) ->
    eunit_data:iter_next(I, fun abort_task/1).

%% @throws abortException()

handle_item(T, St) ->
    case T of
	#test{} -> handle_test(T, St);
	#group{} -> handle_group(T, St)
    end.

handle_test(T, St) ->
    Id = St#procstate.id,
    St#procstate.super ! {test, start, {Id, T#test.module, T#test.line,
					T#test.name, T#test.desc}},
    Status = with_timeout(T#test.timeout, ?DEFAULT_TEST_TIMEOUT,
			  fun () -> run_test(T) end,
			  St),
    St#procstate.super ! {test, done, {Id, Status}},
    ok.

%% @spec (#test{}) -> {ok, Value} | {error, eunit_lib:exception()}
%% @throws eunit_test:wrapperError()

run_test(#test{f = F}) ->
    try eunit_test:run_testfun(F) of
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
    end.

handle_group(T, St0) ->
    St = set_group_order(T, St0),
    Id = St#procstate.id,
    St#procstate.super ! {group, enter, {Id, T#group.desc}},
    run_group(T, St),
    St#procstate.super ! {group, leave, {Id, T#group.desc}},
    ok.

set_group_order(#group{order = undefined}, St) ->
    St;
set_group_order(#group{order = Order}, St) ->
    St#procstate{order = Order}.

run_group(T, St) ->
    Timeout = T#group.timeout,
    case T#group.spawn of
	true ->
	    Child = spawn_group(T, Timeout, St),
	    wait_for_task(Child, St);
	_ ->
	    subtests(T, fun (T) -> group(T, Timeout, St) end)
    end.

spawn_group(T, Timeout, St0) ->
    Fun = fun (St) ->
		  fun () ->
			  subtests(T, fun (T) ->
					      group(T, Timeout, St)
				      end)
		  end
	  end,
    start_task(Fun, St0).

group(T, Timeout, St) ->
    with_timeout(Timeout, ?DEFAULT_GROUP_TIMEOUT,
		 fun () -> tests(T, St) end, St).


%% @throws abortException()

subtests(#group{context = undefined, tests = T}, F) ->
    F(T);
subtests(#group{context = #context{} = C, tests = I}, F) ->
    try
	eunit_data:enter_context(C, I, F)
    catch
	R = setup_failed ->
	    abort_task(R);
	  R = cleanup_failed ->
	    abort_task(R);
	  R = instantiation_failed ->
	    abort_task(R)
    end.
