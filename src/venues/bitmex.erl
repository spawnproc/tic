-module(bitmex).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("bitmex.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(bitmex).

snapshot(_)       -> #shot{}.

name("XBTUSD")    -> bitmex_btc_usd_swap;
name("COIN_BH17") -> bitmex_coin_future;
name("DASH7D")    -> bitmex_dash_futute;
name("ETH7D")     -> bitmex_eth_future;
name(X)           -> [].

subscription()    -> [].

route(#bitmex{table="orderBookL2",action=Ac,data=D}=B,M) ->
    lists:foldl(fun (X,A) -> action(order,B,Ac,X,M) end, [], [X||X<-D]);

route(#bitmex{table="trade",action=Ac,data=D}=B,M) ->
    lists:foldl(fun (X,A) -> action(trade,B,Ac,X,M) end, [], [X||X<-D]);

route(_,M) -> kvs:info(?MODULE,"~p~n",[M]), [].

action(Stream,T,A,#sym{symbol=Sym,side=Side,size=S,price=P,timestamp=TS,id=OID}=Packet,Debug) ->
    trade:trace(?MODULE,[Stream,A,Sym,S,P,Side,Debug,TS,OID]).

trade(Sym,A,"Buy",S,P,M,O)    -> [trade,P,trade:nn(S),bid];
trade(Sym,A,"Sell",S,P,M,O)   -> [trade,P,trade:nn(S),ask].

order(Sym,_,_,S,[],M,O)       -> book:del(#tick{sym=name(Sym),id=O,size=trade:nn(S)});
order(Sym,"delete",_,S,P,M,O) -> book:del(#tick{sym=name(Sym),id=O,size=trade:nn(S),price=P});
order(Sym,_,"Buy",S,P,M,O)    -> book:add(#tick{sym=name(Sym),id=O,size=trade:nn(S),price=P,side=bid});
order(Sym,_,"Sell",S,P,M,O)   -> book:add(#tick{sym=name(Sym),id=O,size=-trade:nn(S),price=P,side=ask}).

state({S,P})      -> {S+1,P}.
print(Msg)        -> route(post(jsone:decode(Msg),#io{}),Msg).
instance()        -> #bitmex{}.
post({Data}, Ctx) -> Bitmex=from_json(Data, instance()),
                     Bitmex#bitmex{data=[ sym:post(I, Ctx) || I <- Bitmex#bitmex.data]}.

init([P], _)                              -> {ok, {1,P}}.
websocket_info(start, _, State)           -> {reply, <<>>, State}.
websocket_handle({pong, _}, _, State)     -> {ok, State};
websocket_handle({text, Msg}, _, State)   -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)       -> print(Msg), {noreply, state(State)}.
websocket_terminate(Msg, _, {_,P})        -> kvs:info(?MODULE,"~p terminated. notify ~p~n",[Msg,P]),
                                             erlang:send_after(100,P,{timer,connect,5}), ok.
