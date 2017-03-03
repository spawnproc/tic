-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(gdax).

snapshot(S)     -> shot:get(S).
subscription()  -> ['BTC-USD', 'BTC-EUR', 'BTC-GBP', 'ETH-BTC', 'ETH-USD'].

name("BTC-USD") -> gdax_btc_usd;
name("BTC-EUR") -> gdax_btc_eur;
name("BTC-GBP") -> gdax_btc_gbp;
name("ETH-BTC") -> gdax_eth_btc;
name("ETH-USD") -> gdax_eth_usd;
name(X)         -> [].

route(#gdax{order_type="limit"},D) ->
    [];

route(#gdax{order_type="market"},D) ->
    [];

route(#gdax{type="match",price=P,side=Side,size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    trade:trace(?MODULE,[trade,A,Sym,S,P,Side,D,T,OID,Seq]);

route(#gdax{type="open",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID,Seq]);

route(#gdax{type="change",price=P,side=Side,new_size=S2,old_size=S1,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    N = trade:print_float(trade:normal(trade:p(abs(trade:nn(S2)-trade:nn(S1))))),
    Normal = case Side of
                  bid -> N;
                  ask -> -N end,
    kvs:info(?MODULE,"change: ~p ~p ~p\r",[Normal,Side,trade:nn(S2)-trade:nn(S1)]),
    trade:trace(?MODULE,[order,A,Sym,Normal,P,Side,D,T,OID,Seq]);

route(#gdax{type="done",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID,Seq]);

route(_,D) -> kvs:info(?MODULE,"~p~n",[D]), [].

trade(Sym,A,"buy",S,P,M,O,Q)      -> [trade,P,trade:nn(S),bid];
trade(Sym,A,"sell",S,P,M,O,Q)     -> [trade,P,trade:nn(S),ask].

order(Sym,"canceled",R,S,P,M,O,Q) -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,"filled",R,S,P,M,O,Q)   -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,A,R,S,P,M,O,Q) when S == 0 orelse P == [] ->
    kvs:info(?MODULE,"if it isn't cancel/filled report error: ~p ~p~n",[M,R]),
                                     book:del(#tick{sym=name(Sym),id=O});
order(Sym,A,"buy",S,P,M,O,Q)      -> book:add(#tick{sym=name(Sym),id=O,size=trade:nn(S),price=P,side=bid,sn=Q});
order(Sym,A,"sell",S,P,M,O,Q)     -> book:add(#tick{sym=name(Sym),id=O,size=-trade:nn(S),price=P,side=ask,sn=Q}).

init([P], _)                            -> subscribe(), {ok, {1,P}}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.
websocket_terminate(Msg, _, {_,P})      -> kvs:info(?MODULE,"terminated ~p. notify ~p~n",[Msg,P]),
                                           erlang:send_after(100,P,{timer,connect,5}), ok.

state({S,P})   -> {S+1,P}.
print(Msg)     -> try route(post(jsone:decode(Msg),#io{}),Msg) catch E:R -> kvs:info(?MODULE,"Error: ~p~n",[{E,R,Msg,erlang:get_stacktrace()}]) end.
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
subscribe()    -> websocket_client:cast(self(),
                  {text, jsone:encode([{type,subscribe},{product_ids,subscription()}])}),
                  websocket_client:cast(self(),{text, jsone:encode([{type,heartbeat},{on,false}])}).
