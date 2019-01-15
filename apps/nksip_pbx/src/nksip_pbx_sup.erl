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

%% @private NkSIP Tutorial main supervisor
-module(nksip_pbx_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(supervisor).

-export([init/1, start_link/0]).


%% @private
start_link() ->
    ChildsSpec = [
        nksip:get_sup_spec(nksip_pbx_server, #{
            plugins => [
                nksip_registrar, nksip_100rel, nksip_gruu,
                nksip_outbound, nksip_timers
            ],
            sip_listen => "<sip:all:5060>, <sip:all:5061;transport=tls>"
        })
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, ChildsSpec}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


