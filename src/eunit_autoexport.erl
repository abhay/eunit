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
