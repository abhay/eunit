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
%%     $Id: eunit_lib.erl,v 1.1 2004/12/04 15:12:36 mremond Exp $
%%
%% @copyright 2004,2005 Mickaël Rémond
%% @version 1.1, {@date} {@time}.
%% @author Mickaël Rémond <mickael.remond@process-one.net>
%%   [http://www.process-one.net/]
%% @see eunit
%% @see eunit_examples
%% @doc Support library for eunit test case writting
%% @end

-module(eunit_lib).

-export([log/4, error/4]).

log(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Tag = lists:concat([File, "(", Line, ")"]),
    Data = io_lib:format(Format, Args),
    error_logger:info_report({Tag, lists:flatten(Data)}).
    
error(Format, Args, LongFile, Line) ->
    File = filename:basename(LongFile),
    Tag = lists:concat([File, "(", Line, ")"]),
    Data = io_lib:format(Format, Args),
    error_logger:info_report({Tag, lists:flatten(Data)}).
