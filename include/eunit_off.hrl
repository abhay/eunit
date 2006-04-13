%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Initial Developer of the Original Code is Richard Carlsson.''
%%
%%     $Id:$
%%

%% including this file turns off testing unless explicitly turned on

%% allow defining EUNIT as a synonym for defining TEST
-ifdef(EUNIT).
-ifndef(TEST).
-define(TEST, true).
-endif.
-endif.

%% set NOTEST unless TEST is defined, then read eunit.hrl
-ifndef(TEST).
-ifndef(NOTEST).
-define(NOTEST, true).
-endif.
-endif.
%% Since this file is normally included with include_lib, it must in its
%% turn use include_lib to read any other header files, at least until
%% the epp include_lib behaviour is fixed.
-include_lib("eunit/include/eunit.hrl").
%%-include("eunit.hrl").
