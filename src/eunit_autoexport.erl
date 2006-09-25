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
    rewrite(Forms, Tests).

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

rewrite([{attribute,_,module,Name}=M | Fs], Exports) ->
    Module = if is_atom(Name) -> Name;
		true -> list_to_atom(packages:concat(Name))
	     end,
    {Fs1, Test} = rewrite(Fs, [], Module, true),
    Es = if Test -> [{test,0} | Exports];
	    true -> Exports
	 end,
    [M, {attribute,0,export,Es} | lists:reverse(Fs1)];
rewrite([F | Fs], Exports) ->
    [F | rewrite(Fs, Exports)];
rewrite([], _Exports) ->
    [].    %% fail-safe, in case there is no module declaration

rewrite([{function,_,test,0,_}=F | Fs], As, Module, _Test) ->
    rewrite(Fs, [F | As], Module, false);
rewrite([F | Fs], As, Module, Test) ->
    rewrite(Fs, [F | As], Module, Test);
rewrite([], As, Module, Test) ->
    {if Test ->
	     [{function,0,test,0,
	       [{clause,0,[],[],
		 [{call,0,{remote,0,{atom,0,eunit},{atom,0,test}},
		   [{atom,0,Module}]}]}]}
	      | As];
	true ->
	     As
     end,
     Test}.
