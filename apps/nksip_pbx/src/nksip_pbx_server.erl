%% -------------------------------------------------------------------
%%
%% Service Callback module
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

%% @doc Service callback module.
%%
%% This module implements the mandatory callback module of each Service application
%%
%% This Service implements a SIP proxy, allowing  endpoints to register 
%% and call each other using its registered uri. 
%% Each registered endpoint's speed is monitored and special "extensions" are
%% available to call all nodes, call the fastest, etc.
%%
%% See {@link //nksip_pbx} for an overview.

-module(nksip_pbx_server).

-export([sip_get_user_pass/4, sip_authorize/3, sip_route/5]).
-export([sip_invite/2]).
-export([sip_dialog_update/3, sip_session_update/3]).
-export([srv_init/2, srv_handle_call/4, srv_handle_cast/3, srv_handle_info/3]).

-define(TIME_CHECK, 10000).

-include_lib("nksip/include/nksip.hrl").
-include_lib("nksip/include/nksip_registrar.hrl").

-include_lib("nkserver/include/nkserver_module.hrl").


%%%%%%%%%%%%%%%%%%%%%%%  NkSIP CallBacks %%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Service Callback: initialization.
%% This function is called by NkSIP after calling `nksip:start/4'.
%% We program a timer to check our nodes.
srv_init(_Spec, State) ->
    erlang:start_timer(?TIME_CHECK, self(), check_speed),
    nkserver:put(?MODULE, speed, []),
    {ok, State#{nksip_pbx=>#{auto_check=>true}}}.


%% @doc Service Callback: Called to check user's password.
%% If the incoming user's realm is one of our domains, the password for any 
%% user is "1234". For other realms, no password is valid.
sip_get_user_pass(_User, <<"nksip">>, _Req, _Call) ->
    <<"1234">>;
sip_get_user_pass(_User, _Realm, _Req, _Call) -> 
    false.


%% @doc Service Callback: Called to check if a request should be authorized.
%% - We first check to see if the request is an in-dialog request, coming from 
%%   the same ip and port of a previously authorized request.
%% - If not, we check if we have a previous authorized REGISTER request from 
%%   the same ip and port.
%% - Next, we check if the request has a valid authentication header with realm 
%%   "nksip". If `{{digest, <<"nksip">>}, true}' is present, the user has 
%%   provided a valid password and it is authorized. 
%%   If `{{digest, <<"nksip">>}, false}' is present, we have presented 
%%   a challenge, but the user has failed it. We send 403.
%% - If no digest header is present, reply with a 407 response sending 
%%   a challenge to the user.

sip_authorize(Auth, Req, _Call) ->
    {ok, Method} = nksip_request:method(Req),
    lager:info("Request ~p auth data: ~p", [Method, Auth]),
    case lists:member(dialog, Auth) orelse lists:member(register, Auth) of
        true -> 
            ok;
        false ->
            case nklib_util:get_value({digest, <<"nksip">>}, Auth) of
                true -> 
                    ok;             % Password is valid
                false -> 
                    forbidden;      % User has failed authentication
                undefined -> 
                    {proxy_authenticate, <<"nksip">>}
                    
            end
    end.


%% @doc Service Callback: Called to decide how to route every new request.
%%
%%  - If the user part of the request-uri is 200, proxy in parallel to all
%%    registered endpoints but me, including a Record-Route header, so
%%    that all dialog requests will go to this proxy.
%%  - If it is 201, call in parallel each two random endpoints, including
%%    a custom header but no Record-Route, so next dialog requests will
%%    go directly to the endpoint.
%%  - For 202, send the request to the fastest registered endpoint.
%%  - For 203, to the slowest.
%%  - If there is a different user part in the request-uri, check to see if 
%%    it is already registered with us and redirect to it.
%%  - If the there is no user part in the request-uri (only the domain) 
%%    process locally if it is one of our domains.
%%    (Since we have not implemented `sip_invite/2', `sip_options/2,' etc., all responses
%%    will be default responses). REGISTER will be processed as configured
%%    when starting the Service.

sip_route(_Scheme, <<"200">>, <<"nksip">>, Req, _Call) ->
    UriList = find_all_except_me(Req),
    {proxy, UriList, [record_route]};

sip_route(_Scheme, <<"201">>, <<"nksip">>, Req, _Call) ->
    All = random_list(find_all_except_me(Req)),
    UriList = take_in_pairs(All),
    {proxy, UriList, [{add, "x-nksip-server", <<"201">>}]};

sip_route(_Scheme, <<"202">>, <<"nksip">>, _Req, _Call) ->
    Speed = nkserver:get(?MODULE, speed),
    UriList = [[Uri] || {_Time, Uri} <- lists:sort(Speed)],
    {proxy, UriList};

sip_route(_Scheme, <<"203">>, <<"nksip">>, _Req, _Call) ->
    Speed = nkserver:get(?MODULE, speed),
    UriList = [[Uri] || {_Time, Uri} <- lists:sort(Speed)],
    {proxy, lists:reverse(UriList)};

sip_route(_Scheme, <<"300">>, <<"nksip">>, _Req, _Call) ->
    process;

% The request is for us
sip_route(_Scheme, <<>>, <<"nksip">>, _Req, _Call) ->
    process;

% The request is for one of our users or a SUBSCRIBE
sip_route(Scheme, User, <<"nksip">>, Req, _Call) ->
    case nksip_request:method(Req) of
        {ok, 'SUBSCRIBE'} ->
            {reply, forbidden};
        _ ->
            UriList = nksip_registrar:find(?MODULE, Scheme, User, <<"nksip">>),
            {proxy, UriList, [record_route]}
    end;

% The request is for another domain, let's proxy it
sip_route(_Scheme, _User, _Domain, Req, _Call) ->
    case nksip_request:is_local_ruri(Req) of
        true ->
            process;
        false ->
            {proxy, ruri, [record_route]}
    end.


sip_dialog_update(Status, Dialog, _Call) ->
    {ok, DialogId} = nksip_dialog:get_handle(Dialog),
    lager:notice("PBX Dialog ~s Update: ~p", [DialogId, Status]),
    ok.


sip_session_update({start, LocalSDP, RemoteSDP}, Dialog, _Call) ->
    {ok, DialogId} = nksip_dialog:get_handle(Dialog),
    lager:notice("PBX Session ~s Start", [DialogId]),
    lager:notice("Local SDP: ~s", [nksip_sdp:unparse(LocalSDP)]),
    lager:notice("Remote SDP: ~s", [nksip_sdp:unparse(RemoteSDP)]),
    ok;

sip_session_update({update, LocalSDP, RemoteSDP}, Dialog, _Call) ->
    {ok, DialogId} = nksip_dialog:get_handle(Dialog),
    lager:notice("PBX Session ~s Update", [DialogId]),
    lager:notice("Local SDP: ~s", [nksip_sdp:unparse(LocalSDP)]),
    lager:notice("Remote SDP: ~s", [nksip_sdp:unparse(RemoteSDP)]),
    ok;

sip_session_update(Status, Dialog, _Call) ->
    {ok, DialogId} = nksip_dialog:get_handle(Dialog),
    lager:notice("PBX Session ~s Update: ~p", [DialogId, Status]),
    ok.


%% Called when extension 300 is called
%% - Generates fake SDP, as inactive
%% - Sends a 180 Ringing (using reliable provisional responses if possible)
%% - Sends a 200 OK after 5 seconds
sip_invite(Req, _Call) ->
    {ok, Body} = nksip_request:body(Req),
    case nksip_sdp:is_sdp(Body) of
        true ->
            {ok, Handle} = nksip_request:get_handle(Req),
            SDP1 = Body#sdp{vsn=Body#sdp.vsn+1000},
            SDP2 = nksip_sdp:update(SDP1, inactive),
            spawn(
                fun() ->
                    nksip_request:reply(rel_ringing, Handle),
                    timer:sleep(5000),
                    nksip_request:reply({ok, [{body, SDP2}]}, Handle)
                end),
            noreply;
        false ->
            {reply, forbidden}
    end.


%% @doc Service Callback: Synchronous user call.
srv_handle_call(get_speed, _From, _Service, State) ->
    Speed = nkserver:get(?MODULE, speed),
    Reply = [{Time, nklib_unparse:uri(Uri)} || {Time, Uri} <- Speed],
    {reply, Reply, State}.


%% @doc Service Callback: Asynchronous user cast.
srv_handle_cast({speed_update, Speed}, _Service, State) ->
    ok =  nkserver:put(?MODULE, speed, Speed),
    erlang:start_timer(?TIME_CHECK, self(), check_speed),
    {noreply, State};

srv_handle_cast({check_speed, true}, Service, State) ->
    srv_handle_info({timeout, none, check_speed}, Service,
                        State#{nksip_pbx=>#{auto_check=>true}});

srv_handle_cast({check_speed, false}, _Service, State) ->
    {noreply, State#{nksip_pbx=>#{auto_check=>false}}}.


%% @doc Service Callback: External erlang message received.
%% The programmed timer sends a `{timeout, _Ref, check_speed}' message
%% periodically to the Service.
srv_handle_info({timeout, _, check_speed}, _Service, #{nksip_pbx:=#{auto_check:=true}}=State) ->
    Self = self(),
    spawn(fun() -> test_speed(Self) end),
    {noreply, State};

srv_handle_info({timeout, _, check_speed}, _Service, #{nksip_pbx:=#{auto_check:=false}}=State) ->
    {noreply, State}.



%%%%%%%%%%%%%%%%%%%%%%%  Internal %%%%%%%%%%%%%%%%%%%%%%%%


%% @doc Gets all registered contacts and sends an OPTION to each of them
%% to measure its response time.
test_speed(Pid) ->
    Speed = test_speed(find_all(), []),
    gen_server:cast(Pid, {speed_update, Speed}).

%% @private
test_speed([], Acc) ->
    Acc;
test_speed([Uri|Rest], Acc) ->
    case timer:tc(fun() -> nksip_uac:options(?MODULE, Uri, []) end) of
        {Time, {ok, 200, []}} -> 
            test_speed(Rest, [{Time/1000, Uri}|Acc]);
        {_, _} -> 
            test_speed(Rest, Acc)
    end.


%% @doc Gets all registered contacts
find_all() ->
    All = [
        [Uri || #reg_contact{contact=Uri} <- List] 
        || {_, _, List} <- nksip_registrar_util:get_all()
    ],
    lists:flatten(All).


%% @doc Gets all registered contacts, excluding the one in `Request'
find_all_except_me(ReqId) ->
    {ok, [From]} = nksip_request:header(<<"from">>, ReqId),
    [{Scheme, User, Domain}] = nksip_parse:aors(From),
    AOR = {Scheme, User, Domain},
    All = [
        [Uri || #reg_contact{contact=Uri} <- List] 
        || {_, R_AOR, List} <- nksip_registrar_util:get_all(), R_AOR /= AOR
    ],
    lists:flatten(All).



%%%%%%%%%%%%%%%%%%%%%%%  Utilities %%%%%%%%%%%%%%%%%%%%%%%%


%% @private
random_list(List) ->
    List1 = [{nklib_util:rand(1, length(List)+1), Term} || Term <- List],
    [Term || {_, Term} <- lists:sort(List1)].


%% @private
take_in_pairs([]) -> [];
take_in_pairs(List) -> take_in_pairs(List, []).

take_in_pairs([], Acc) -> lists:reverse(Acc);
take_in_pairs([Last], Acc) -> take_in_pairs([], [[Last]|Acc]);
take_in_pairs([One, Two|Rest], Acc) -> take_in_pairs(Rest, [[One, Two]|Acc]).
