%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sponge_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start(_Type, _Args) ->
    Dispatch = [{'_', [{'_', websocket_handler, []}]}],
    cowboy:start_listener(my_http_listener, 5,
                          cowboy_tcp_transport, [{port, 8765}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ).

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
