%%%-------------------------------------------------------------------
%%% @author David Haglund
%%% @copyright (C) 2012, David Haglund
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------

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
    lists:flatten(io_lib:format(
                    "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                    [Year, Month, Day, Hour, Min, Sec])).
