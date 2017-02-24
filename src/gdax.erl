-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(gdax).

init([], _)                             -> subscribe(), {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State) -> State + 1.
print(Msg)   -> kvs:info(?MODULE,"~p~n", [post(?JSON:decode(Msg),#ctx{})]).
subscribe()  -> websocket_client:cast(self(), {text, <<"{\"type\":\"subscribe\",\"product_ids\": [\"BTC-USD\"]}">>}).

instance() -> #gdax{}.
post({Data},#ctx{user=User}) -> {ok, from_json(Data, instance()) }.
