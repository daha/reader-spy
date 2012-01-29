%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(websocket_handler).

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

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
    {ok, Req, undefined, hibernate}.

websocket_handle({text, Json}, Req, State) ->
    Proplist = [{binary_to_atom(K, utf8), V} ||
                   {K, V} <- tuple_to_list(jsonerl:decode(Json))],
    io:format("Json=~p prolist=~p~n", [Json, Proplist]),
    {ok, Req, State};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
