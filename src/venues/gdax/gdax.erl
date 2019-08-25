-module(gdax).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("gdax.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(gdax).

subscription()  -> ['BTC-USD', 'ETH-BTC', 'BTC-EUR', 'BTC-GBP', 'ETH-USD'].
tables()        -> [gdax_btc_usd,gdax_btc_eur,gdax_btc_gbp,gdax_eth_btc,gdax_eth_usd,io,order,tick].

name('BTC-USD') -> gdax_btc_usd;
name('BTC-EUR') -> gdax_btc_eur;
name('BTC-GBP') -> gdax_btc_gbp;
name('ETH-BTC') -> gdax_eth_btc;
name('ETH-USD') -> gdax_eth_usd;
name(X) when is_list(X) -> name(list_to_existing_atom(X));
name(tick)      -> tick;
name(X)         -> [].

route(#gdax{order_type="limit",product_id=Sym,order_id=O},D) ->
    [];

route(#gdax{order_type="market",product_id=Sym,order_id=O},D) ->
    [];

route(#gdax{type="match",price=P,side=Side,size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    tic:trace(?MODULE,[trade,A,Sym,S,P,Side,D,T,OID,Seq]);

route(#gdax{type="open",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    tic:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID,Seq]);

route(#gdax{type="change",price=P,side=Side,new_size=S2,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    kvs:info(?MODULE,"Change Order: ~p~n",[G]),
    book:del(#tick{id=OID,sym=name(Sym)}),
    tic:trace(?MODULE,[order,A,Sym,S2,P,Side,D,T,OID,Seq]);

route(#gdax{type="done",price=P,side=Side,remaining_size=S,reason=A,product_id=Sym,time=T,order_id=OID,sequence=Seq}=G,D) ->
    tic:trace(?MODULE,[order,A,Sym,S,P,Side,D,T,OID,Seq]);

route(_,D) -> kvs:info(?MODULE,"~p~n",[D]), [].

trade(Sym,A,"buy",S,P,M,O,Q)      -> [trade,P,tic:nn(S),bid];
trade(Sym,A,"sell",S,P,M,O,Q)     -> [trade,P,tic:nn(S),ask];
trade(Sym,A,R,S,P,M,O,Q)          -> kvs:info(?MODULE,"Warning. Reason is empty: ~p~n",[{Sym,A,R,S,P,O,Q}]),
                                     [].

order(Sym,"canceled",R,S,P,M,O,Q)  -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,"filled",R,S,P,M,O,Q)   -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,A,R,S,P,M,O,Q) when S == [] orelse P == [] ->
    kvs:info(?MODULE,"if it isn't cancel/filled report error: ~p ~p~n",[M,R]),
                                     book:del(#tick{sym=name(Sym),id=O});
order(Sym,A,"buy",S,P,M,O,Q)      -> book:add(#tick{sym=name(Sym),id=O,size=tic:nn(S),price=P,side=bid,sn=Q});
order(Sym,A,"sell",S,P,M,O,Q)     -> book:add(#tick{sym=name(Sym),id=O,size=-tic:nn(S),price=P,side=ask,sn=Q}).

state({S,P})   -> {S+1,P}.
instance()     -> #gdax{}.
post({Data},_) -> from_json(Data, instance()).
subscribe()    -> websocket_client:cast(self(),{text,jsone:encode([{type,subscribe},{product_ids,subscription()}])}).
heart_off()    -> websocket_client:cast(self(),{text,jsone:encode([{type,heartbeat},{on,false}])}).
print(Msg)     -> try ?MODULE:route(post(jsone:decode(Msg),#io{}),Msg)
                  catch E:R -> kvs:info(?MODULE,"Error: ~p~n",[{E,R,Msg,erlang:get_stacktrace()}]) end.

init([P], _)                            -> [ mnesia:clear_table(X) || X <- tables() ], subscribe(), heart_off(),
                                           application:set_env(tic,gdax,self()),
                                           [ self() ! {left, Symbol, []} || Symbol <- subscription() ],
                                           {ok, {1,P}}.
websocket_info({_, Sym, []}, _, S)      ->       snapshot:sync(?MODULE,Sym),  {ok, S};
websocket_info({left, Sym, Pid}, _, S)  -> Pid ! snapshot:sync(?MODULE,Sym),  {ok, S};
websocket_info({right, Sym, Pid}, _, S) -> Pid ! snapshot:check(?MODULE,Sym), {ok, S};
websocket_info(start, _, State)         -> {reply, <<>>, State}.
websocket_handle({pong, _}, _, State)   -> {ok, State};
websocket_handle({text, Msg}, _, State) -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)     -> print(Msg), {noreply, State}.
websocket_terminate(Msg, _, {_,P})      -> kvs:info(?MODULE,"terminated ~p. notify ~p~n",[Msg,P]),
                                           erlang:send_after(100,P,{timer,connect,5}), ok.

current(Topic,Asks,Bids,Seq) ->
    [ [ case kvs:index(order,uid,O) of
             [Ord] -> book:del(setelement(1,#tick{id=O,sym=name(Topic)},name(Topic))),
                      order(Topic,"book",side(Side),tic:normal(S),tic:normal(P),[],O,kvs:next_id(order,1));
                [] -> order(Topic,"book",side(Side),tic:normal(S),tic:normal(P),[],O,kvs:next_id(order,1))
        end
        || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

temp(Asks,Bids,Seq) ->
    [ [ order(tick,"book",side(Side),tic:normal(S),tic:normal(P),[],O,kvs:next_id(order,1))
        || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

side(bid)    -> "buy";
side(ask)    -> "sell".

left_cut(Topic)  -> cut(Topic, fun left/2, fun left/4).
right_cut(Topic) -> cut(Topic, fun right/2, fun right/4).

right(X,Seq) -> X#order.sn >= Seq.
left(X,Seq)  -> X#order.sn < Seq.
right(Topic,Asks,Bids,Seq)  -> temp(Asks,Bids,Seq), {snapshot:book(?MODULE,Topic),snapshot:book(?MODULE,tick),Asks++Bids}.
left(Topic,Asks,Bids,Seq)   -> current(Topic,Asks,Bids,Seq), {snapshot:book(?MODULE,Topic),[]}.

cut(Topic, F, G) ->
    #gshot{sequence=Seq,bids=Bids,asks=Asks} = gshot:get(Topic), Name = name(Topic),
    {Orders,_} = lists:partition(fun(X) -> F(X,Seq) end, kvs:index(order,sym,Name)),
    kvs:info(?MODULE,"Dead Orders: ~p Topic: ~p Seq: ~p~n",[Orders,Topic,Seq]),
    Deleted = lists:map(fun(#order{uid=O,sym=Sym}) -> book:del(#tick{sym=Sym,id=O}) end, Orders),
    G(Topic,Asks,Bids,Seq).

shotlevel(Shot,Price) -> [ [tic:normal(P),tic:normal(S),I] || [P,S,I] <- Shot, tic:normal(P) == Price ].
