%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
%%% @todo Store the events on disk (disklog?)
%%% @todo use the ip and the request as file-name

-module(websocket_handler).

-include_lib("cowboy/include/http.hrl").

-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).

-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
         websocket_info/3, websocket_terminate/3]).

-record(state, {path="",
                disklog
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
    Path = Req#http_req.raw_path,
    PathStr = string:strip(binary_to_list(Path), left, $/),
    File = filename:join("data", PathStr),
    filelib:ensure_dir(File),
    DiskLogOptions = [{name, Path},
                      {file, File},
                      {size, infinity},
                      {mode, read_write},
                      {type, halt},
                      {format, internal}],
    Disklog = case disk_log:open(DiskLogOptions) of
                  {ok, Disklog0} -> Disklog0;
                  {repaired, Disklog0, _, _} -> Disklog0;
                  {error, Error} -> erlang:error(Error)
              end,
    {ok, Req, #state{path = Path, disklog = Disklog}, hibernate}.

websocket_handle({text, JsonStr}, Req, State) ->
    DecodedJson = jsonerl:decode(JsonStr),
    ok = disk_log:log(State#state.disklog, DecodedJson),
    io:format("~p~n", [JsonStr]),
    {ok, Req, State};
websocket_handle(_Any, Req, State) ->
    {ok, Req, State}.

websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.
