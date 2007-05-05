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
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@it.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Interpretation of symbolic test representation

-module(eunit_data).

-include("eunit.hrl").
-include("eunit_internal.hrl").

-export([list/1, iter_init/2, iter_next/2, iter_prev/2, iter_id/1, 
	 list_size/1, enter_context/3]).

-import(lists, [foldr/3]).

%% TODO: document {application,...}, and xxx_tests modules

%% @type tests() =
%%            SimpleTest
%%          | [tests()]
%%          | moduleName()
%%          | {module, moduleName()}
%%          | {application, appName()}
%%          | {application, appName(), [term()]}
%%          | fileName()
%%          | {file, fileName()}
%%          | {string(), tests()}
%%          | {generator, () -> tests()}
%%          | {generator, M::moduleName(), F::functionName()}
%%          | {spawn, tests()}
%%          | {spawn, Node::atom(), tests()}
%%          | {timeout, T::number(), tests()}
%%          | {inorder, tests()}
%%          | {inparallel, tests()}
%%          | {inparallel, N::integer(), tests()}
%%          | {with, X::any(), [AbstractTestFunction]}
%%          | {setup, Where::local | spawn | {spawn, Node::atom()},
%%                    Setup::() -> (R::any()),
%%                    Cleanup::(R::any()) -> any(),
%%                    tests() | Instantiator
%%            }
%%          | {setup, Setup, Cleanup, tests() | Instantiator}
%%          | {setup, Where, Setup, tests() | Instantiator}
%%          | {setup, Setup, tests() | Instantiator}
%%          | {foreach, Where::local | spawn | {spawn, Node::atom()},
%%                      Setup::() -> (R::any()),
%%                      Cleanup::(R::any()) -> any(),
%%                      [tests() | Instantiator]
%%            }
%%          | {foreach, Setup, Cleanup, [tests() | Instantiator]}
%%          | {foreach, Where, Setup, [tests() | Instantiator]}
%%          | {foreach, Setup, [tests() | Instantiator]}
%%          | {foreachx, Where::local | spawn | {spawn, Node::atom()},
%%                       SetupX::(X::any()) -> (R::any()),
%%                       CleanupX::(X::any(), R::any()) -> any(),
%%                       Pairs::[{X::any(),
%%                                (X::any(), R::any()) -> tests()}]
%%            }
%%          | {foreachx, SetupX, CleanupX, Pairs}
%%          | {foreachx, Where, SetupX, Pairs}
%%          | {foreachx, SetupX, Pairs}
%%          | {node, Node::atom(), tests() | Instantiator}
%%          | {node, Node, Args::string(), tests() | Instantiator}
%%
%% SimpleTest = TestFunction | {Line::integer(), SimpleTest}
%%
%% TestFunction = () -> any()
%%              | {M::moduleName(), F::functionName()}.
%%
%% AbstractTestFunction = (X::any()) -> any()
%%
%% Instantiator = (R::any()) -> tests()
%%              | {with, [AbstractTestFunction]}
%%
%% Note that `{string(), ...}' is a short-hand for `{string(), {...}}'
%% if the tuple contains more than two elements.
%%
%% @type moduleName() = atom()
%% @type functionName() = atom()
%% @type appName() = atom()
%% @type fileName() = string()

%% ---------------------------------------------------------------------
%% Abstract test set iterator

-record(iter,
	{prev = [],
	 next = [],
	 tests = [],
	 pos = 0,
	 parent = []}).

%% @spec (tests(), [integer()]) -> testIterator()
%% @type testIterator()

iter_init(Tests, ParentID) ->
    #iter{tests = Tests, parent = lists:reverse(ParentID)}.

%% @spec (testIterator()) -> [integer()]

iter_id(#iter{pos = N, parent = Ns}) ->
    lists:reverse(Ns, [N]).

%% @throws {bad_test, term()}
%%       | {generator_failed, exception()}
%%       | {no_such_function, eunit_lib:mfa()}
%%       | {module_not_found, moduleName()}
%%       | {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

%% @spec (testIterator(), Handler) -> none | {testItem(), testIterator()}
%%    Handler = (term()) -> term()

iter_next(I, H) ->
    iter_do(fun iter_next/1, I, H).

iter_do(F, I, H) ->
    try F(I)
    catch
	R = {bad_test, _Bad} ->
	    H(R);
	R = {generator_failed, _Exception} ->
	    H(R);
	R = {no_such_function, _MFA} ->
	    H(R);
	R = {module_not_found, _M} ->
	    H(R);
	R = {file_read_error, _Info} ->
	    H(R)
    end.

iter_next(I = #iter{next = []}) ->
    case next(I#iter.tests) of
	{T, Tests} ->
	    {T, I#iter{prev = [T | I#iter.prev],
		       tests = Tests,
		       pos = I#iter.pos + 1}};
	none ->
	    none
    end;
iter_next(I = #iter{next = [T | Ts]}) ->
    {T, I#iter{next = Ts,
	       prev = [T | I#iter.prev],
	       pos = I#iter.pos + 1}}.

%% @spec (testIterator(), Handler) -> none | {testItem(), testIterator()}
%%    Handler = (term()) -> term()

iter_prev(I, H) ->
    iter_do(fun iter_prev/1, I, H).

iter_prev(#iter{prev = []}) ->
    none;
iter_prev(#iter{prev = [T | Ts]} = I) ->
    {T, I#iter{prev = Ts,
	       next = [T | I#iter.next],
		       pos = I#iter.pos - 1}}.


%% ---------------------------------------------------------------------
%% Concrete test set representation iterator

%% @spec (tests()) -> none | {testItem(), tests()}
%% @type testItem() = #test{} | #group{}
%% 
%% @throws {bad_test, term()}
%%       | {generator_failed, eunit_lib:exception()}
%%       | {no_such_function, eunit_lib:mfa()}
%%       | {module_not_found, moduleName()}

next(Tests) ->
    case eunit_lib:dlist_next(Tests) of
	[T | Ts] ->
	    case parse(T) of
		{data, T1} ->
		    next([T1 | Ts]);
		T1 ->
		    {T1, Ts}
	    end;
	[] ->
	    none
    end.

%% this returns either a #test{} or #group{} record, or {data, T} to
%% signal that T has been substituted for the given representation

parse({foreach, S, Fs}) when is_function(S), is_list(Fs) ->
    parse({foreach, S, fun ok/1, Fs});
parse({foreach, S, C, Fs})
  when is_function(S), is_function(C), is_list(Fs) ->
    parse({foreach, ?DEFAULT_SETUP_PROCESS, S, C, Fs});
parse({foreach, P, S, Fs})
  when is_function(S), is_list(Fs) ->
    parse({foreach, P, S, fun ok/1, Fs});
parse({foreach, P, S, C, Fs} = T)
  when is_function(S), is_function(C), is_list(Fs) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    case Fs of
	[F | Fs1] ->
	    {data, [{setup, P, S, C, F}, {foreach, S, C, Fs1}]};
	[] ->
	    {data, []}
    end;
parse({foreachx, S1, Ps}) when is_function(S1), is_list(Ps) ->
    parse({foreachx, S1, fun ok/2, Ps});
parse({foreachx, S1, C1, Ps})
  when is_function(S1), is_function(C1), is_list(Ps) ->
    parse({foreachx, ?DEFAULT_SETUP_PROCESS, S1, C1, Ps});
parse({foreachx, P, S1, Ps})
  when is_function(S1), is_list(Ps) ->
    parse({foreachx, P, S1, fun ok/2, Ps});
parse({foreachx, P, S1, C1, Ps} = T) 
  when is_function(S1), is_function(C1), is_list(Ps) ->
    check_arity(S1, 1, T),
    check_arity(C1, 2, T),
    case Ps of
	[{X, F1} | Ps1] when is_function(F1) ->
	    check_arity(F1, 2, T),
	    S = fun () -> S1(X) end,
	    C = fun (R) -> C1(X, R) end,
	    F = fun (R) -> F1(X, R) end,
	    {data, [{setup, P, S, C, F}, {foreachx, S1, C1, Ps1}]};
	[_|_] ->
	    bad_test(T);
	[] ->
	    {data, []}
    end;
parse({generator, F} = T) when is_function(F) ->
    check_arity(F, 0, T),
    %% use run_testfun/1 to handle wrapper exceptions
    case eunit_test:run_testfun(F) of
	{ok, T1} ->
	    {data, T1};
	{error, {Class, Reason, Trace}} ->
	    throw({generator_failed, {Class, Reason, Trace}})
    end;
parse({generator, M, F}) when is_atom(M), is_atom(F) ->
    parse({generator, eunit_test:function_wrapper(M, F)});
parse({inorder, T}) ->
    group(#group{tests = T, order = inorder});
parse({inparallel, T}) ->
    parse({inparallel, 0, T});
parse({inparallel, N, T}) when is_integer(N), N >= 0 ->
    group(#group{tests = T, order = {inparallel, N}});
parse({timeout, N, T}) when is_number(N), N >= 0 ->
    group(#group{tests = T, timeout = round(N * 1000)});
parse({spawn, T}) ->
    group(#group{tests = T, spawn = local});
parse({spawn, N, T}) when is_atom(N) ->
    group(#group{tests = T, spawn = {remote, N}});
parse({setup, S, I}) when is_function(S) ->
    parse({setup, S, fun ok/1, I});
parse({setup, S, C, I}) when is_function(S), is_function(C) ->
    parse({setup, ?DEFAULT_SETUP_PROCESS, S, C, I});
parse({setup, P, S, I}) when is_function(S) ->
    parse({setup, P, S, fun ok/1, I});
parse({setup, P, S, C, I} = T)
  when is_function(S), is_function(C), is_function(I) ->
    check_arity(S, 0, T),
    check_arity(C, 1, T),
    case erlang:fun_info(I, arity) of
	{arity, 0} ->
	    %% if I is nullary, it is a plain test
	    parse({setup, S, C, fun (_) -> I end});
	_ ->
	    %% otherwise, I must be an instantiator function
	    check_arity(I, 1, T),
	    case P of
		local -> ok;
		spawn -> ok;
		{spawn, N} when is_atom(N) -> ok;
		_ -> bad_test(T)
	    end,
	    group(#group{tests = I,
			 context = #context{setup = S, cleanup = C,
					    process = P}})
    end;
parse({setup, P, S, C, {with, As}}) when is_list(As) ->
    parse({setup, P, S, C, fun (X) -> {with, X, As} end});
parse({setup, P, S, C, T}) when is_function(S), is_function(C) ->
    parse({setup, P, S, C, fun (_) -> T end});
parse({node, N, T}) when is_atom(N) ->
    parse({node, N, "", T});
parse({node, N, A, T1}=T) when is_atom(N) ->
    case eunit_lib:is_string(A) of
	true ->
	    %% TODO: better stack traces for internal funs like these
	    parse({setup,
		   fun () ->
			   %% TODO: auto-start net_kernel if needed
 			   StartedNet = false,
%% The following is commented out because of problems when running
%% eunit as part of the init sequence (from the command line):
%% 			   StartedNet =
%% 			       case whereis(net_kernel) of
%% 				   undefined ->
%% 				       M = list_to_atom(atom_to_list(N)
%% 							++ "_master"),
%% 				       case net_kernel:start([M]) of
%% 					   {ok, _} ->
%% 					       true;
%% 					   {error, E} ->
%% 					       throw({net_kernel_start, E})
%% 				       end;
%% 				   _ -> false
%% 			       end,
%% 			   eunit:debug({started, StartedNet}),
			   {Name, Host} = eunit_lib:split_node(N),
			   {ok, Node} = slave:start_link(Host, Name, A),
			   {Node, StartedNet}
		   end,
		   fun ({Node, StopNet}) ->
%% 			   eunit:debug({stop, StopNet}),
			   slave:stop(Node),
			   case StopNet of
			       true -> net_kernel:stop();
			       false -> ok
			   end
		   end,
		   T1});
	false ->
	    bad_test(T)
    end;
parse({module, M}) when is_atom(M) ->
    {data, {"module '" ++ atom_to_list(M) ++ "'", get_module_tests(M)}};
parse({application, A}) when is_atom(A) ->
    {data, {file, atom_to_list(A)++".app"}};
parse({application, A, Info}=T) when is_atom(A) ->
    case proplists:get_value(modules, Info) of
	Ms when is_list(Ms) ->
	    case [M || M <- Ms, not is_atom(M)] of
		[] ->
		    {data, {"application '" ++ atom_to_list(A) ++ "'", Ms}};
		_ ->
		    bad_test(T)
	    end;
	_ ->
	    bad_test(T)
    end;
parse({file, F} = T) when is_list(F) ->
    case eunit_lib:is_string(F) of
	true ->
	    {data, {"file \"" ++ F ++ "\"", get_file_tests(F)}};
	false ->
	    bad_test(T)
    end;
parse({with, X, As}=T) when is_list(As) ->
    case As of
	[A | As1] ->
	    check_arity(A, 1, T),
	    {data, [fun () -> A(X) end, {with, X, As1}]};
	[] ->
	    {data, []}
    end;
parse({S, T1} = T) when is_list(S) ->
    case eunit_lib:is_string(S) of
	true ->
	    group(#group{tests = T1, desc = S});
	false ->
	    bad_test(T)
    end;
parse(T) when is_tuple(T), size(T) > 2, is_list(element(1, T)) ->
    [S | Es] = tuple_to_list(T),
    parse({S, list_to_tuple(Es)});
parse(M) when is_atom(M) ->
    parse({module, M});
parse(T) when is_list(T) ->
    case eunit_lib:is_string(T) of
	true ->
	    parse({file, T});
	false ->
	    bad_test(T)
    end;
parse(T) ->
    parse_simple(T).

%% parse_simple always produces a #test{} record

parse_simple({L, F}) when is_integer(L), L >= 0 ->
    (parse_simple(F))#test{line = L};
parse_simple(F) ->
    parse_function(F).

parse_function(F) when is_function(F) ->
    check_arity(F, 0, F),
    {module, M} = erlang:fun_info(F, module),
    #test{f = F, module = M, name = eunit_lib:fun_parent(F)};
parse_function({M,F}) when is_atom(M), is_atom(F) ->
    #test{f = eunit_test:function_wrapper(M, F), module = M, name = F};
parse_function(F) ->
    bad_test(F).

check_arity(F, N, T) when is_function(F) ->
    case erlang:fun_info(F, arity) of
	{arity, N} ->
	    ok;
	_ ->
	    bad_test(T) 
    end;
check_arity(_, _, T) ->
    bad_test(T).

bad_test(T) ->
    throw({bad_test, T}).

ok(_) -> ok.    
ok(_, _) -> ok.    

%% This does some look-ahead and folds nested groups and tests where
%% possible. E.g., {String, Test} -> Test#test{desc = String}.

group(#group{context = #context{}} = G) ->
    %% leave as it is - the test body is an instantiator, which is not
    %% suitable for lookahead (and anyway, properties of the setup
    %% should not be merged with properties of its body, e.g. spawn)
    G;
group(#group{tests = T0, desc = Desc, order = Order, context = Context,
	     spawn = Spawn, timeout = Timeout} = G) ->
    {T1, Ts} = lookahead(T0),
    {T2, _} = lookahead(Ts),
    case T1 of
	#test{desc = Desc1, timeout = Timeout1}
	when T2 == none, Spawn == undefined, Context == undefined,
	     ((Desc == undefined) or (Desc1 == undefined)),
	     ((Timeout == undefined) or (Timeout1 == undefined)) ->
	    %% a single test within a non-spawn/setup group: put the
	    %% information directly on the test; drop the order
	    T1#test{desc = join_properties(Desc, Desc1),
		    timeout = join_properties(Timeout, Timeout1)};
	#group{desc = Desc1, order = Order1, context = Context1,
	       spawn = Spawn1, timeout = Timeout1}
	when T2 == none,
	     ((Desc == undefined) or (Desc1 == undefined)),
	     ((Order == undefined) or (Order1 == undefined)),
	     ((Context == undefined) or (Context1 == undefined)),
	     ((Spawn == undefined) or (Spawn1 == undefined)),
	     ((Timeout == undefined) or (Timeout1 == undefined)) ->
	    %% two nested groups with non-conflicting properties
	    T1#group{desc = join_properties(Desc, Desc1),
		     order = join_properties(Order, Order1),
		     context = join_properties(Context, Context1),
		     spawn = join_properties(Spawn, Spawn1),
		     timeout = join_properties(Timeout, Timeout1)};
	_ ->
	    %% leave as it is and discard the lookahead
	    G
    end.

lookahead(T) ->
    case next(T) of
	{T1, Ts} -> {T1, Ts};
	none -> {none, []}
    end.

join_properties(undefined, X) -> X;    
join_properties(X, undefined) -> X.


%% ---------------------------------------------------------------------
%% Extracting test funs from a module

%% @throws {module_not_found, moduleName()}

get_module_tests(M) ->
    TestSuffix = ?DEFAULT_TEST_SUFFIX,
    GeneratorSuffix = ?DEFAULT_GENERATOR_SUFFIX,
    try M:module_info(exports) of
	Es ->
	    Fs = testfuns(Es, M, TestSuffix, GeneratorSuffix),
	    Name = atom_to_list(M),
	    case lists:suffix(?DEFAULT_TESTMODULE_SUFFIX, Name) of
		false ->
		    Name1 = Name ++ ?DEFAULT_TESTMODULE_SUFFIX,
		    M1 = list_to_atom(Name1),
		    try get_module_tests(M1) of
			Fs1 ->
			    Fs ++ [{"module '" ++ Name1 ++ "'", Fs1}]
		    catch
			{module_not_found, M1} ->
			    Fs
		    end;
		true ->
		    Fs
	    end
    catch
	error:undef -> 
	    throw({module_not_found, M})
    end.

testfuns(Es, M, TestSuffix, GeneratorSuffix) ->
    foldr(fun ({F, 0}, Fs) ->
		  N = atom_to_list(F),
		  case lists:suffix(TestSuffix, N) of
		      true ->
			  [{M,F} | Fs];
		      false ->
			  case lists:suffix(GeneratorSuffix, N) of
			      true ->
				  [{generator, M, F} | Fs];
			      false ->
				  Fs
			  end
		  end;
	      (_, Fs) ->
		  Fs
	  end,
	  [],
	  Es).    


%% ---------------------------------------------------------------------
%% Reading tests from a file

%% @throws {file_read_error, {Reason::atom(), Message::string(),
%%                            fileName()}}

get_file_tests(F) ->
    eunit_lib:consult_file(F).


%% ---------------------------------------------------------------------
%% Entering a setup-context, with guaranteed cleanup.

%% @spec (Tests::#context{}, Instantiate, Callback) -> any()
%%    Instantiate = (any()) -> tests()
%%    Callback = (tests()) -> any()
%% @throws {ErrorType, eunit_lib:exception()}
%% ErrorType = setup_failed | instantiation_failed | cleanup_failed

enter_context(#context{setup = S, cleanup = C, process = P}, I, F) ->
    F1 = case P of
	     local -> F;
	     spawn -> fun (X) -> F({spawn, X}) end;
	     {spawn, N} -> fun (T) -> F({spawn, N, T}) end
	 end,
    eunit_test:enter_context(S, C, I, F1).


%% ---------------------------------------------------------------------
%% Returns a symbolic listing of a set of tests
%%
%% @type testInfoList() = [Entry]
%%   Entry = {item, testId(), Description, testName()}
%%         | {group, testId(), Description, testInfoList}
%%   Description = string()
%% @type testId() = [integer()]
%% @type testName() = {moduleName(), functionName()}
%%		    | {moduleName(), functionName(), lineNumber()}
%% @type lineNumber() = integer().  Proper line numbers are always >= 1.

%% @throws {error, Reason::term()}

list(T) ->
    list(T, []).

list(T, ParentID) ->
    list_loop(iter_init(T, ParentID)).

list_loop(I) ->
    case iter_next(I, fun (R) -> throw({error, R}) end) of
 	{T, I1} ->
	    Id = iter_id(I1),
 	    case T of
		#test{} ->
		    Name = case T#test.line of
			       0 -> {T#test.module, T#test.name};
			       Line -> {T#test.module, T#test.name, Line}
			   end,
		    [{item, Id, desc_string(T#test.desc), Name}
		     | list_loop(I1)];
		#group{context = Context} ->
		    [{group, Id, desc_string(T#group.desc),
		      list_context(Context, T#group.tests, Id)}
		     | list_loop(I1)]
	    end;
 	none ->
 	    []
    end.

desc_string(undefined) -> "";
desc_string(S) -> S.

list_context(undefined, T, ParentId) ->
    list(T, ParentId);
list_context(#context{process = local}, T, ParentId) ->
    browse_context(T, fun (T) -> list(T, ParentId) end);
list_context(#context{process = spawn}, T, ParentId) ->
    browse_context(T, fun (T) -> list({spawn, T}, ParentId) end);
list_context(#context{process = {spawn, N}}, T, ParentId) ->
    browse_context(T, fun (T) -> list({spawn, N, T}, ParentId) end).

browse_context(T, F) ->
    try
 	eunit_test:browse_context(T, F)
    catch
 	R = instantiation_failed ->
 	    throw(R)
    end.

list_size({item, _, _, _}) -> 1;
list_size({group, _, _, Es}) -> list_size(Es);    
list_size(Es) when is_list(Es) ->
    lists:foldl(fun (E, N) -> N + list_size(E) end, 0, Es).

-ifdef(TEST).

generator_exported_() ->
    generator().

generator() ->
    T = ?_test(ok),
    [T, T, T].

echo_proc() ->
    receive {P,X} -> P ! X, echo_proc() end.

ping(P) ->
    P ! {self(),ping}, receive ping -> ok end.    

data_test_() ->
    Setup = fun () -> spawn(fun echo_proc/0) end,
    Cleanup = fun (Pid) -> exit(Pid, kill) end,
    Fail = ?_test(throw(eunit)),
    T = ?_test(ok),
    Tests = [T,T,T],
    [?_assertMatch(ok, eunit:test(T)),
     ?_assertMatch(error, eunit:test(Fail)),
     ?_assertMatch(ok, eunit:test({generator, fun () -> Tests end})),
     ?_assertMatch(ok, eunit:test({generator, fun generator/0})),
     ?_assertMatch(ok, eunit:test({generator, ?MODULE, generator_exported_})),
     ?_assertMatch(ok, eunit:test({inorder, Tests})),
     ?_assertMatch(ok, eunit:test({inparallel, Tests})),
     ?_assertMatch(ok, eunit:test({timeout, 10, Tests})),
     ?_assertMatch(ok, eunit:test({spawn, Tests})),
     ?_assertMatch(ok, eunit:test({setup, Setup, Cleanup,
				   fun (P) -> ?_test(ok = ping(P)) end})),
     %%?_assertMatch(ok, eunit:test({node, test@localhost, Tests})),
     ?_assertMatch(ok, eunit:test({module, eunit_lib})),
     ?_assertMatch(ok, eunit:test(eunit_lib)),
     ?_assertMatch(ok, eunit:test("examples/tests.txt"))

     %%?_test({foreach, Setup, [T, T, T]})
    ].

lazy_test_() ->
    {spawn, [?_test(undefined = put(count, 0)),
	     lazy_gen(100),
	     ?_assertMatch(100, get(count))]}.

lazy_gen(N) ->
    {generator,
     fun () ->
	     if N > 0 ->
		     [?_test(put(count,1+get(count)))
		      | lazy_gen(N-1)];
		true ->
		     []
	     end
     end}.
-endif.
