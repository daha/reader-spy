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
-module(sponge).

%% API
-export([start/0]).
-export([stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(cowboy),
    start_cowboy_listener().

stop([OtherNode]) ->
    case net_adm:ping(OtherNode) of
        pong ->
            ok = rpc:call(OtherNode, init, stop, []);
        pang ->
            io:format("There is no node named ~p~n", [OtherNode])
    end,
    init:stop().

%%%===================================================================
%%% Internal
%%%===================================================================

start_cowboy_listener() ->
    Dispatch = [{'_', [{'_', websocket_handler, []}]}],
    cowboy:start_listener(my_http_listener, 5,
                          cowboy_tcp_transport, [{port, 8765}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]).
