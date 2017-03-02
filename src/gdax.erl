-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(gdax).

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

route(#gdax{type="match",price=P,side=Side,size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[trade,A,Sym,S,P,Side,D,T,OID]);

route(#gdax{type="open",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID]);

route(#gdax{type="change",price=P,side=Side,new_size=S2,old_size=S1,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    Normal = trade:normal(trade:p(trade:nn(S2)-trade:nn(S1))),
    io:format("change: ~p\r",[Normal]),
    trade:trace(?MODULE,[order,A,Sym,Normal,P,Side,D,T,OID]);

route(#gdax{type="done",price=P,side=Side,size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID]);

route(_,D) -> kvs:info(?MODULE,"~p",[D]), [].

trade(Sym,A,"buy",S,P,M,O)      -> [trade,P,trade:nn(S),bid];
trade(Sym,A,"sell",S,P,M,O)     -> [trade,P,trade:nn(S),ask].

order(Sym,_,_,S,[],M,O)         -> book:del(#tick{sym=name(Sym),id=O,size=trade:nn(S)});
order(Sym,"canceled",_,S,P,M,O) -> book:del(#tick{sym=name(Sym),id=O,size=trade:nn(S),price=P});
order(Sym,_,"buy",S,P,M,O)      -> book:add(#tick{sym=name(Sym),id=O,size=trade:nn(S),price=P,side=bid});
order(Sym,_,"sell",S,P,M,O)     -> book:add(#tick{sym=name(Sym),id=O,size=-trade:nn(S),price=P,side=ask}).

init([P], _)                            -> subscribe(), {ok, {1,P}}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, {_,P})        -> kvs:info(?MODULE,"terminated ~p",[P]),  ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state({S,P})   -> {S+1,P}.
print(Msg)     -> route(post(jsone:decode(Msg),#io{}),Msg).
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
subscribe()    -> websocket_client:cast(self(),
                  {text, jsone:encode([{type,subscribe},
                                       {product_ids,['BTC-USD',
                                                     'BTC-GBP','ETH-USD',
                                                     'ETH-BTC','BTC-EUR' ]}])}).
