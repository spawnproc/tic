-module(gdax).
-behaviour(websocket_client_handler).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init([], _)                             -> subscribe(), {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};% end;
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State) -> State + 1.
print(Msg)   -> io:format("Received msg ~p~n", [Msg]).
subscribe()  -> websocket_client:cast(self(), {text, <<"{\"type\":\"subscribe\",\"product_ids\": [\"BTC-USD\"]}">>}).
