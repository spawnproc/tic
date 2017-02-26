-module(bitmex).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("bitmex.hrl").
-include("core.hrl").
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3,post/2,order/7]).
-compile({parse_transform, rest}).
-rest_record(bitmex).

route(#bitmex{table=T,action=Ac,data=D}=B,M) ->
    lists:foldl(fun (X,A) -> action(B,Ac,X,M) end, [], [X||X<-D]).

action(T,A,#sym{symbol=Sym,side=Side,size=S,price=P}=SymRec,Debug) ->
    trade:order_trace(?MODULE,[A,Sym,S,P,Side,SymRec]).

order(_,"delete",_,_,S,_,M)         -> [0,"-0"];
order(_,_,"Buy",_,S,[],M) when S >0 -> [0,lists:concat(["+",S])];
order(_,_,_,_,S,[],M)               -> [0,lists:concat(["-",S])];
order(_,_,"Buy",_,S,P,M) when S > 0 -> [0,lists:concat(["+",S]),P];
order(_,_,_,_,S,P,M)                -> [0,lists:concat(["-",S]),P].

state(State)      -> State + 1.
print(Msg)        -> route(post(jsone:decode(Msg),#ctx{}),Msg).
instance()        -> #bitmex{}.
post({Data}, Ctx) -> Bitmex=from_json(Data, instance()),
                     Bitmex#bitmex{data=[ sym:post(I, Ctx) || I <- Bitmex#bitmex.data]}.

init([], _)                               -> {ok, 1, 100}.
websocket_info(start, _, State)           -> {reply, <<>>, State}.
websocket_terminate(_, _, _)              -> kvs:info(?MODULE,"terminated",[]), ok.
websocket_handle({pong, _}, _, State)     -> {ok, State};
websocket_handle({text, Msg}, _, State)   -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)       -> print(Msg), {noreply, state(State)}.
