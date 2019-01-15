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

-module(nksip_pbx).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([check_speed/1, get_speed/0, start_trace/0, stop_trace/0, loglevel/1]).


%% @doc Stops or restart automatic response time detection.
check_speed(Bool) ->
    gen_server:cast(nksip_pbx_server, {check_speed, Bool}).


%% @doc Get all registered endpoints with their last response time.
get_speed() ->
     gen_server:call(nksip_pbx_server, get_speed).



%% @doc Enables SIP trace messages to console.
start_trace() ->
    nksip_trace:start(nksip_pbx_server).


stop_trace() ->
    nksip_trace:stop(nksip_pbx_server).



%% @doc Changes console log level.
%% Available options are `debug' (maximum), `info' (medium) and `notice' (minimum).
-spec loglevel(debug|info|notice) ->
    ok.

loglevel(debug) ->
    nklib_log:console_loglevel(debug),
    ok = nksip:update(nksip_pbx_server, #{sip_debug=>[call]});

loglevel(Level) ->
    nklib_log:console_loglevel(Level),
    ok.



