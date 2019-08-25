-module(bitmex).
-behaviour(rest).
-behaviour(websocket_client_handler).
-include("bitmex.hrl").
-include("core.hrl").
-compile(export_all).
-export([init/2,websocket_handle/3,websocket_info/3,websocket_terminate/3]).
-compile({parse_transform, rest}).
-rest_record(bitmex).

name('XBTUSD')    -> bitmex_btc_usd_swap;
name('COIN_BH17') -> bitmex_coin_future;
name('DASH7D')    -> bitmex_dash_future;
name('ETH7D')     -> bitmex_eth_future;
name(X) when is_list(X) -> name(list_to_atom(X));
name(tick)        -> tick;
name(X)           -> [].

subscription()    -> [].
tables()          -> [bitmex_coin_future,bitmex_btc_usd_swap,
                      bitmex_dash_future,bitmex_eth_future].

route(#bitmex{table="orderBookL2",action=Ac,data=D}=B,M) ->
    lists:foldl(fun (X,A) -> action(order,B,Ac,X,M) end, [], [X||X<-D]);

route(#bitmex{table="trade",action=Ac,data=D}=B,M) ->
    lists:foldl(fun (X,A) -> action(trade,B,Ac,X,M) end, [], [X||X<-D]);

route(A,M) -> kvs:info(?MODULE,"~p ~p~n",[A,M]), [].

action(Stream,T,A,#bsym{symbol=Sym,side=Side,size=S,price=P,timestamp=TS,id=OID}=Packet,Debug) ->
    tic:trace(?MODULE,[Stream,A,Sym,S,P,Side,Debug,TS,OID,OID]).

trade(Sym,A,"Buy",S,P,M,O,Q)    -> [trade,P,tic:nn(S),bid];
trade(Sym,A,"Sell",S,P,M,O,Q)   -> [trade,P,tic:nn(S),ask];
trade(Sym,A,R,S,P,M,O,Q)        -> kvs:info(?MODULE,"Warning. Reason is empty: ~p~n",[{Sym,A,R,S,P,O,Q}]),
                                   [].

order(Sym,"delete",_,S,P,M,O,Q) -> book:del(#tick{sym=name(Sym),id=O});
order(Sym,"update",D,S,P,M,O,Q) -> case book:del(#tick{id=O,sym=name(Sym)}) of
                                        [Price,UID] -> order(Sym,"insert",D,S,Price,M,O,Q);
                                        [] -> [] end;
order(Sym,A,R,S,P,M,O,Q) when S == 0 orelse P == [] ->
    kvs:info(?MODULE,"if it isn't cancel/filled report error: ~p~n",[{A,M}]),
                                   book:del(#tick{sym=name(Sym),id=O});
order(Sym,A,"Buy",S,P,M,O,Q)    -> book:add(#tick{sym=name(Sym),id=O,size=tic:nn(S),price=P,side=bid,sn=Q});
order(Sym,A,"Sell",S,P,M,O,Q)   -> book:add(#tick{sym=name(Sym),id=O,size=-tic:nn(S),price=P,side=ask,sn=Q}).

state({S,P})      -> {S+1,P}.
instance()        -> #bitmex{}.
post({Data}, Ctx) -> Bitmex=from_json(Data, instance()),
                     Bitmex#bitmex{data=[ bsym:post(I, Ctx) || I <- Bitmex#bitmex.data]}.
print(Msg)        -> try route(post(jsone:decode(Msg),#io{}),Msg)
                     catch E:R -> kvs:info(?MODULE,"Error: ~p~n",[{E,R,Msg,erlang:get_stacktrace()}]) end.

init([P], _)                              -> [ mnesia:clear_table(X) || X <- tables() ],
                                             application:set_env(tic,?MODULE,self()),
                                             {ok, {1,P}}.
websocket_info(start, _, State)           -> {reply, <<>>, State};
websocket_info({left, Sym, Pid}, _, S)    -> Pid ! snapshot:sync(?MODULE,Sym),  {ok, S};
websocket_info({right, Sym, Pid}, _, S)   -> Pid ! snapshot:check(?MODULE,Sym), {ok, S};
websocket_info(start, _, State)           -> {reply, <<>>, State}.
websocket_handle({pong, _}, _, State)     -> {ok, State};
websocket_handle({text, Msg}, _, State)   -> print(Msg), {ok, state(State)};
websocket_handle(Msg, _Conn, State)       -> print(Msg), {noreply, state(State)}.
websocket_terminate(Msg, _, {_,P})        -> kvs:info(?MODULE,"~p terminated. notify ~p~n",[Msg,P]),
                                             erlang:send_after(100,P,{timer,connect,5}), ok.

left_cut(Topic)  -> {[],[],[]}.
right_cut(Topic) ->
    Shot0 = bshot:get(Topic), Name = name(Topic),
    Seq =  lists:max([ Id || #order{sn=Id} <- kvs:index(order,sym,Name) ]),
    kvs:info(?MODULE,"Max Order Id ~p~n",[Seq]),
    {Shot,_} = lists:partition(fun(X) -> X#bshot.id =< Seq end, Shot0),
    [ order(tick,"book",Side,tic:normal(tic:p(S)),tic:normal(tic:p(P)),[],O,kvs:next_id(order,1)) || {_,O,Side,S,P,Sym} <- Shot ],
    {snapshot:book(?MODULE,Topic),snapshot:book(?MODULE,tick),Shot}.

shotlevel(Shot,Price) -> [ [tic:normal(tic:p(P)),tic:normal(tic:p(S)),O] || {_,O,Side,S,P,Sym} <- Shot, tic:normal(tic:p(P)) == Price ].
