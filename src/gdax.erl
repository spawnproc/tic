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

route(#gdax{size=S, price=P, side=Side, reason=A, product_id=Sy}=GDAX,Debug) ->
     {{Y,M,D},_} = calendar:universal_time(),
     FileName = lists:concat(["priv/",?MODULE,"/",Y,"-",M,"-",D,"-",Sy]),
     Order = list_to_binary(sym:f(order(Sy,A,Side,f(S),S,P,Debug))),
     file:write_file(FileName, Order, [raw, binary, append, read, write]),
     [].

f([]) -> 0;
f(X) -> list_to_float(X).

order(_,"canceled",_,_,_,_,M)                             -> [0,"-0"];
order(_,_,_,_,[],P,M)                                     -> [0,P];
order(_,_,"buy",S,SS,[],M) when is_float(S) andalso S > 0 -> [0,lists:concat(["+",SS])];
order(_,_,_,S,SS,[],M)                                    -> [0,SS];
order(_,_,"buy",S,SS,P,M) when is_float(S) andalso S > 0  -> [0,lists:concat(["+",SS]),P];
order(_,_,_,S,SS,P,M) when S > 0                          -> [0,lists:concat(["+",SS]),P];
order(_,_,_,S,SS,P,M)                                     -> [0,SS,P];
order(Sym,A,Side,S,SS,P,M)                                -> [Sym,A,Side,S,SS,P,M]. % default all

init([], _)                             -> subscribe(), {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State)   -> State + 1.
print(Msg)     -> route(post(?JSON:decode(Msg),#ctx{}),Msg).
subscribe()    -> websocket_client:cast(self(), {text, <<"{\"type\":\"subscribe\",\"product_ids\": [\"BTC-USD\"]}">>}).
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
