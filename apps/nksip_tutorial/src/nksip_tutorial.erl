%% -------------------------------------------------------------------
%%
%% Copyright (c) 2019 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Companion code for NkSIP Tutorial.


-module(nksip_tutorial).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([launch/0, start_trace/0, stop_trace/0, loglevel/1]).



%% @doc Launches the full tutorial in one shot
launch() ->
    nksip_registrar:clear(nksip_tutorial_server),

    {ok,200,[]} = nksip_uac:options(nksip_tutorial_client2, "sip:127.0.0.1:5070", []),

    {ok,407,[{reason_phrase, <<"Proxy Authentication Required">>}]} =
        nksip_uac:options(nksip_tutorial_client1, "sip:127.0.0.1", [{get_meta,[reason_phrase]}]),

    {ok,200,[]} = nksip_uac:options(nksip_tutorial_client1, "sip:127.0.0.1", [{sip_pass, "1234"}]),
    {ok,200,[]} = nksip_uac:options(nksip_tutorial_client2, "<sip:127.0.0.1;transport=tls>", [{sip_pass, "1234"}]),

    {ok,200,[{<<"contact">>, [<<"<sip:nksip_tutorial_client1@localhost:5070>", _/binary>>]}]} =
        nksip_uac:register(nksip_tutorial_client1, "sip:127.0.0.1",
                           [{sip_pass, "1234"}, contact, {get_meta, [<<"contact">>]}]),

    {ok,200,[]} = nksip_uac:register(nksip_tutorial_client2, "sips:127.0.0.1", [{sip_pass, "1234"}, contact]),

    {ok,200,[{all_headers, _}]} = 
        nksip_uac:register(nksip_tutorial_client2, "sips:127.0.0.1", [{sip_pass, "1234"}, {get_meta, [all_headers]}]),

    {ok,200,[]} = nksip_uac:options(nksip_tutorial_client1, "sip:127.0.0.1", []),
    {ok,200,[]} = nksip_uac:options(nksip_tutorial_client2, "sips:127.0.0.1", []),

    {ok,407,[]} = nksip_uac:options(nksip_tutorial_client1, "sips:nksip_tutorial_client2@nksip", [{route, "<sip:127.0.0.1;lr>"}]),
    {ok,200,[{<<"x-nk-id">>, [<<"nksip_tutorial_client2">>]}]} =
        nksip_uac:options(nksip_tutorial_client1, "sips:nksip_tutorial_client2@nksip",
                          [{route, "<sip:127.0.0.1;lr>"}, {sip_pass, "1234"},
                           {get_meta, [<<"x-nk-id">>]}]),

    {ok,488,[]} = 
        nksip_uac:invite(nksip_tutorial_client2, "sip:nksip_tutorial_client1@nksip", [{route, "<sips:127.0.0.1;lr>"}]),

    {ok,200,[{dialog, DlgId}]}= 
        nksip_uac:invite(nksip_tutorial_client2, "sip:nksip_tutorial_client1@nksip",
                        [{route, "<sips:127.0.0.1;lr>"}, {body, nksip_sdp:new()},
                          auto_2xx_ack]),

    {ok, confirmed} = nksip_dialog:get_meta(invite_status, DlgId),
    [_, _, _] = nksip_dialog:get_all_data(),
    timer:sleep(1000),

    {ok,200,[]} = nksip_uac:bye(DlgId, []).



%% ===================================================================
%% Utilities
%% ===================================================================

%% @doc Enables SIP trace messages to console.
start_trace() ->
    nksip_trace:start(nksip_tutorial_server),
    nksip_trace:start(nksip_tutorial_client1),
    nksip_trace:start(nksip_tutorial_client2).


stop_trace() ->
    nksip_trace:stop(nksip_tutorial_server),
    nksip_trace:stop(nksip_tutorial_client1),
    nksip_trace:stop(nksip_tutorial_client2).



%% @doc Changes console log level.
%% Available options are `debug' (maximum), `info' (medium) and `notice' (minimum).
-spec loglevel(debug|info|notice) -> 
    ok.

loglevel(debug) ->
    nklib_log:console_loglevel(debug),
    ok = nksip:update(nksip_tutorial_server, #{sip_debug=>[call]}),
    ok = nksip:update(nksip_tutorial_client1, #{sip_debug=>[call]}),
    ok = nksip:update(nksip_tutorial_client2, #{sip_debug=>[call]});

loglevel(Level) ->
    nklib_log:console_loglevel(Level),
    ok.



