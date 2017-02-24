-module(bitmex).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("bitmex.hrl").
-include("core.hrl").
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(bitmex).

init([], _)                             -> {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State) -> State + 1.
print(Msg)   -> kvs:info(?MODULE,"~p~n", [post(?JSON:decode(Msg),#ctx{})]).

instance() -> #bitmex{}.
post({Data}, Ctx) ->
    Bitmex=from_json(Data, instance()),
    #bitmex{data=D}=Bitmex,
    {ok, Bitmex#bitmex{data=[ sym:post(I, Ctx) || I <- D]}}.
