-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3,order/7,post/2]).
-compile({parse_transform, rest}).
-rest_record(gdax).

route(#gdax{type="open",price=P,side=Side,remaining_size=S,reason=A,product_id=Sy},D) ->
    trade:order_trace(?MODULE,[A,Sy,S,P,Side,D]);

route(#gdax{type="change",price=P,side=Side,new_size=S,reason=A,product_id=Sy},D) ->
    trade:order_trace(?MODULE,[A,Sy,S,P,Side,D]);

route(#gdax{type=T,size=S,price=P,side=Side,reason=A,product_id=Sy},D) ->
    trade:order_trace(?MODULE,[A,Sy,S,P,Side,D]).

order(_,[],_,0,_,_,M)                                     -> [0,"-0"];
order(_,"canceled",_,_,_,_,M)                             -> [0,"-0"];
order(_,_,_,_,[],P,M)                                     -> [0,P];
order(_,_,"buy",S,SS,[],M) when is_float(S) andalso S > 0 -> [0,lists:concat(["+",SS])];
order(_,_,_,S,SS,[],M)                                    -> [0,SS];
order(_,_,"buy",S,SS,P,M) when is_float(S) andalso S > 0  -> [0,lists:concat(["+",SS]),P];
order(_,_,_,S,SS,P,M) when S > 0                          -> [0,lists:concat(["+",SS]),P];
order(_,_,_,S,SS,P,M)                                     -> [0,SS,P].

init([], _)                             -> subscribe(), {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> kvs:info(?MODULE,"terminated",[]), ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State)   -> State + 1.
print(Msg)     -> route(post(jsone:decode(Msg),#ctx{}),Msg).
subscribe()    -> websocket_client:cast(self(), {text, <<"{\"type\":\"subscribe\",\"product_ids\": [\"BTC-USD\"]}">>}).
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
