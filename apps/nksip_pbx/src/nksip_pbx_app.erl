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

%% @doc NkSIP OTP Application Module
-module(nksip_pbx_app).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(application).

-export([start/0, start/2, stop/1]).

-define(APP, nksip_pbx).


%% ===================================================================
%% Private
%% ===================================================================

%% @doc Starts NkSIP stand alone.
-spec start() -> 
    ok | {error, Reason::term()}.

start() ->
    application:ensure_all_started(?APP).


%% @private OTP standard start callback
start(_Type, _Args) ->
    nksip_pbx_sup:start_link().



%% @private OTP standard stop callback
stop(_) ->
    ok.


