-module(client).
-behaviour(websocket_client_handler).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init([], _ConnState) -> {ok, 1, 1000}.
websocket_info(start, _ConnState, State) -> {reply, <<>>, State}.
websocket_terminate(Reason, _ConnState, State) -> ok.
websocket_handle({pong, _Msg}, _ConnState, State) -> {ok, State};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg ~p~n", [Msg]),
    case State of
         1 -> {reply, {text, <<"{\"op\": \"subscribe\", \"args\": [\"trade\"]}">>},      state(State)};
         2 -> {reply, {text, <<"{\"op\": \"subscribe\", \"args\": [\"insurance\"]}">>},  state(State)};
         3 -> {reply, {text, <<"{\"op\": \"subscribe\", \"args\": [\"instrument\"]}">>}, state(State)};
         _ -> {noreply, State + 1} end.
state(State) -> State + 1.
