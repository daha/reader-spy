%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
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
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ).
