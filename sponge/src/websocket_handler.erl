%%%====================================================================
%%% Copyright (c) 2012, David Haglund
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%%     * Redistributions of source code must retain the above
%%%       copyright notice, this list of conditions and the following
%%%       disclaimer.
%%%
%%%     * Redistributions in binary form must reproduce the above
%%%       copyright notice, this list of conditions and the following
%%%       disclaimer in the documentation and/or other materials
%%%       provided with the distribution.
%%%
%%%     * Neither the name of the copyright holder nor the names of
%%%       its contributors may be used to endorse or promote products
%%%       derived from this software without specific prior written
%%%       permission.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS
%%% BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
%%% EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
%%% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
%%% TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
%%% THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%====================================================================
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%%====================================================================

-module(websocket_handler).

-include_lib("cowboy/include/http.hrl").

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {io_device
               }).

init({_Any, http}, _Req, []) ->
    %% assume 'Upgrade' is requested.
    {upgrade, protocol, cowboy_http_websocket}.

handle(Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(
                   200, [{'Content-Type', <<"text/html">>}], <<>>, Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.

websocket_init(_Any, Req, []) ->
    ReqPath = Req#http_req.raw_path,
    File = get_filename(ReqPath),
    filelib:ensure_dir(File),
    FileOptions = [write, raw],
    {ok, IoDevice} = file:open(File, FileOptions),
    {ok, Req, #state{io_device = IoDevice}, hibernate}.

websocket_handle({text, JsonStr}, Req, State) ->
    ok = file:write(State#state.io_device, [JsonStr, "\n"] ),
    {ok, Req, State};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.

get_filename(ReqPath) ->
    ReqPathStr = string:strip(binary_to_list(ReqPath), left, $/),
    {ok, Hostname} = inet:gethostname(),
    File = string:join([Hostname, ReqPathStr, timestamp_to_string()], "-"),
    filename:join("data", File).

timestamp_to_string() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:universaltime(),
    {_, _, Us} = now(),
    lists:flatten(io_lib:format(
                    "~4.10.0B-~2.10.0B-~2.10.0B"
                    "T~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec, Us])).
