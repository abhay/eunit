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
%% The Initial Developer of the Original Code is Mickaël Rémond.''
%%
%%     $Id: eunit.hrl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% Macros
-define(log(Format,Args), eunit_lib:log(Format,Args,?FILE,?LINE)).
-define(error(Format,Args), eunit_lib:error(Format,Args,?FILE,?LINE)).

-define(match(ExpectedRes,Expr),
        fun() ->
		ActualRes = (catch (Expr)),
		case ActualRes of
		    ExpectedRes -> ok;
		    _ -> exit(test_case_failed)
		end
	end()).

-define(match_inverse(NotExpectedRes,Expr),
        fun() ->
		ActualRes = (catch (Expr)),
		case ActualRes of
		    NotExpectedRes ->
			exit(test_case_failed);
		    _ -> ok
		end
	end()).
