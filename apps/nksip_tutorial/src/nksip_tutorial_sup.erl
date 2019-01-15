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
-module(nksip_tutorial_sup).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-behaviour(supervisor).

-export([init/1, start_link/0]).


%% @private
start_link() ->
    ChildsSpec = [
        nksip:get_sup_spec(nksip_tutorial_server, #{
            sip_local_host => "localhost",
            plugins => [nksip_registrar],
            sip_listen => "sip:all:5060, <sip:all:5061;transport=tls>"
        }),
        nksip:get_sup_spec(nksip_tutorial_client1, #{
            sip_local_host => "localhost",
            sip_from => "sip:nksip_tutorial_client1@nksip",
            plugins => [nksip_uac_auto_auth],
            sip_listen => "sip:127.0.0.1:5070, sips:127.0.0.1:5071"
        }),
        nksip:get_sup_spec(nksip_tutorial_client2, #{
            sip_local_host => "localhost",
            sip_from => "sips:nksip_tutorial_client2@nksip",
            plugins => [nksip_uac_auto_auth],
            sip_listen => "sip:all:5080, sips:all:5081"
        })
    ],
    supervisor:start_link({local, ?MODULE}, ?MODULE, {{one_for_one, 10, 60}, ChildsSpec}).


%% @private
init(ChildSpecs) ->
    {ok, ChildSpecs}.


