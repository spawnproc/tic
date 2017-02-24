-module(bitmex).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("bitmex.hrl").
-include("core.hrl").
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3,post/2]).
-compile({parse_transform, rest}).
-rest_record(bitmex).

route(#bitmex{table=T,action=Ac,data=D},M)-> lists:foldl(fun (X,A) -> action(Ac,X,M) end, [], [X||X<-D]).
action(A,#sym{symbol=Sy,side=Side,size=S,price=P}=Sym,Debug) ->
     {{Y,M,D},_}=calendar:universal_time(),
     FileName = lists:concat(["priv/",?MODULE,"/",Y,"-",M,"-",D,"-",Sy]),
     Order = list_to_binary(sym:f(order(Sy,A,Side,S,f(P),Debug))),
     file:write_file(FileName, Order, [raw, binary, append, read, write]),
     [].

f(X) when is_integer(X) -> integer_to_list(X);
f(X) when is_float(X) -> float_to_list(X,[{decimals,9},compact]);
f(X) -> X.

order(_,"delete",_,S,_,M)         -> [0,"-0"];
order(_,_,"Buy",S,[],M) when S >0 -> [0,lists:concat(["+",S])];
order(_,_,_,S,[],M)               -> [0,lists:concat(["-",S])];
order(_,_,"Buy",S,P,M) when S > 0 -> [0,lists:concat(["+",S]),P];
order(_,_,_,S,P,M)                -> [0,lists:concat(["-",S]),P];
order(Sym,A,Side,S,P,M)           -> [Sym,A,Side,S,P]. % default all

state(State)      -> State + 1.
print(Msg)        -> route(post(?JSON:decode(Msg),#ctx{}),Msg).
instance()        -> #bitmex{}.
post({Data}, Ctx) -> Bitmex=from_json(Data, instance()), #bitmex{data=D}=Bitmex,
                     Bitmex#bitmex{data=[ sym:post(I, Ctx) || I <- D]}.

init([], _)                               -> {ok, 1, 100}.
websocket_info(start, _, State)           -> {reply, <<>>, State}.
websocket_terminate(_, _, _)              -> ok.
websocket_handle({pong, _}, _, State)     -> {ok, State};
websocket_handle({text, Msg}, _, State)   -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)       -> print(Msg), {noreply, state(State)}.
