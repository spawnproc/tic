-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(gdax).

name("BTC-USD") -> btc_usd;
name("BTC-EUR") -> btc_eur;
name("BTC-GBP") -> btc_gpb;
name("ETH-BTC") -> eth_btc;
name("ETH-USD") -> eth_usd;
name(X) -> [].

route(#gdax{order_type="limit"},D) -> ok;

route(#gdax{type="match",price=P,side=Side,size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[trade,A,Sym,S,P,Side,D,T,OID]);

route(#gdax{type="open",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID]);

route(#gdax{type="change",price=P,side=Side,new_size=S,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID]);

route(#gdax{size=S,price=P,side=Side,reason=A,product_id=Sym,time=T,order_id=OID},D) ->
    trade:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID]).

trade(Sym,A,"buy",S,P,M,O)      -> [trade,P,book:cat(S,bid)];
trade(Sym,A,"sell",S,P,M,O)     -> [trade,P,book:cat(S,ask)].

order(Sym,_,_,_,[],M,O)         -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,"canceled",_,_,P,M,O) -> book:del(#tick{sym=name(Sym),price=P,id=O});
order(Sym,_,"buy",S,P,M,O)      -> book:add(#tick{sym=name(Sym),price=P,id=O,size=  trade:nn(S), side=bid});
order(Sym,_,"sell",S,P,M,O)     -> book:add(#tick{sym=name(Sym),price=P,id=O,size=- trade:nn(S), side=ask}).

init([], _)                             -> subscribe(), {ok, 1, 100}.
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_terminate(_, _, _)            -> kvs:info(?MODULE,"terminated",[]), ok.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.

state(State)   -> State + 1.
print(Msg)     -> route(post(jsone:decode(Msg),#ctx{}),Msg).
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
subscribe()    -> websocket_client:cast(self(),
                  {text, jsone:encode([{type,subscribe},
                                       {product_ids,['BTC-USD','BTC-EUR',
                                                     'BTC-GBP','ETH-USD',
                                                     'ETH-BTC','BTC-EUR','ETH-EUR' ]}])}).
