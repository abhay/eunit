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
%% File: eunit_autoexport.erl
%%
%% $Id:$ 
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2006 Richard Carlsson
%% @private
%% @see eunit
%% @doc Parse transform for automatic exporting of test functions.

-module(eunit_autoexport).

-include("eunit_internal.hrl").

-export([parse_transform/2]).


parse_transform(Forms, Options) ->
    TestSuffix = proplists:get_value(test_suffix, Options,
				     ?DEFAULT_TEST_SUFFIX),
    GeneratorSuffix = proplists:get_value(generator_suffix, Options,
					  ?DEFAULT_GENERATOR_SUFFIX),
    F = fun (Form, Set) ->
		form(Form, Set, TestSuffix, GeneratorSuffix)
	end,
    Tests = sets:to_list(lists:foldl(F, sets:new(), Forms)),
    export(Forms, Tests).

form({function, _L, Name, 0, _Cs}, Tests, TestSuffix, GeneratorSuffix) ->
    N = atom_to_list(Name),
    case lists:suffix(TestSuffix, N) of
	true ->
	    sets:add_element({Name, 0}, Tests);
	false ->
	    case lists:suffix(GeneratorSuffix, N) of
		true ->
		    sets:add_element({Name, 0}, Tests);
		false ->
		    case lists:suffix("_exported_", N) of
			true ->
			    sets:add_element({Name, 0}, Tests);
			false ->
			    Tests
		    end
	    end
    end;
form(_, Tests, _, _) ->
    Tests.

export([{attribute,L,module,_}=M | Fs], Names) ->
    [M, {attribute,L,export,Names} | Fs];
export([F | Fs], Names) ->
    [F | export(Fs, Names)];
export([], _Names) ->
    [].  % just a sane fallback for safety
