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
-export([start_permanent/0]).

-export([stop/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_permanent() ->
    start(permanent).

start() ->
    start(transient).

start(Type) ->
    application:start(cowboy, Type),
    application:start(sponge, Type).

stop([OtherNode]) ->
    case net_adm:ping(OtherNode) of
        pong ->
            rpc:call(OtherNode, init, stop, []);
        pang ->
            io:format("There is no node named ~p~n", [OtherNode])
    end,
    init:stop().
