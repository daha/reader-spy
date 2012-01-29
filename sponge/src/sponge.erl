%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(sponge).
-behaviour(application).

%% API
-export([start/0]).
-export([start/2, stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    application:start(cowboy),
    application:start(sponge).


start(_Type, _Args) ->
    Dispatch = [{'_', [{'_', websocket_handler, []}]}],
    cowboy:start_listener(my_http_listener, 100,
                          cowboy_tcp_transport, [{port, 8765}],
                          cowboy_http_protocol, [{dispatch, Dispatch}]
                         ).
    % TODO: move cowboy in under a supervisor
    % sponge_sup:start_link().

stop(_State) ->
    ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
