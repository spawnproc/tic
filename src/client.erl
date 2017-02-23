-module(client).
-behaviour(websocket_client_handler).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).

init([], _ConnState) ->
    websocket_client:cast(self(), {text, <<"\"op\": \"subscribe\", \"args\": [\"trade\"]}">>}),
    {ok, 2, 1000}.

websocket_handle({pong, _Msg}, _ConnState, State) ->
    io:format("Received pong ~n"),
    Proto = websocket_req:protocol(_ConnState),
    {ok, State};
websocket_handle({text, Msg}, _ConnState, 5) ->
    io:format("Received msg ~p~n", [Msg]),
    {close, <<>>, 10};
websocket_handle({text, Msg}, _ConnState, State) ->
    io:format("Received msg ~p~n", [Msg]),
    {noreply, State + 1}.

websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    ok.
