-module(shot).
-behaviour(rest).
-include("core.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/1]).
-rest_record(shot).

get(Tick) ->
    ret(httpc:request(get,
        {lists:concat(["https://api.gdax.com/products/",Tick,"/book?level=3"]),
        [{"User-Agent","ticker"}]},[],[])).

post({D}) -> from_json(D,#shot{}).

ret({ok,{{_,C,_},_,A}}) when C>=200, C<300 -> post(jsone:decode(list_to_binary(A)));
ret({ok,{S,_,B}})                          -> #shot{};
ret(Error)                                 -> #shot{}.

side(bid)    -> "buy";
side(ask)    -> "sell".
sync(Topic)  -> cut(Topic, fun left/2, fun left/4).
check(Topic) -> cut(Topic, fun right/2, fun right/4).
right(X,Seq) -> X#order.sn > Seq.
left(X,Seq)  -> X#order.sn < Seq.
right(Topic,Asks,Bids,Seq)  -> temp(Asks,Bids,Seq),          {book(Topic),book(tick),Asks++Bids}.
left(Topic,Asks,Bids,Seq)   -> current(Topic,Asks,Bids,Seq), {book(Topic),[]}.
order(Topic,Side,S,P,O,Seq) -> gdax:order(Topic,"book",side(Side),trade:normal(S),trade:normal(P),[],O,Seq).

book(Topic) -> [ X || X <- kvs:all(gdax:name(Topic)), element(#tick.size,X) /= 0].

current(Topic,Asks,Bids,Seq) ->
    [ [ case kvs:get(order,O) of
             {ok, Ord} -> [];
             {error, _} -> order(Topic,Side,S,P,O,Seq) end
        || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

temp(Asks,Bids,Seq) ->
    [ [ order(tick,Side,S,P,O,Seq)
        || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

cut(Topic, F, G) ->
    #shot{sequence=Seq,bids=Bids,asks=Asks} = shot:get(Topic), Name = gdax:name(Topic),
    kvs:info(?MODULE,"Sequence: ~p~n",[Seq]),
    {Orders,_} = lists:partition(fun(X) -> F(X,Seq) end, kvs:index(order,sym,Name)),
    kvs:info(?MODULE,"Orders to Delete: ~p on topic ~p~n",[Orders,Topic]),
    Deleted = lists:map(fun(#order{uid=O,sym=Sym}) -> book:del(#tick{sym=Sym,id=O}) end, Orders),
    G(Topic,Asks,Bids,Seq).

sort(B1)          -> lists:sort(fun(X,Y) -> element(#tick.price,X) > element(#tick.price,Y) end,B1).
ok(A,B,Shot)      -> ok1(sort(B),sort(A),0,Shot).
ok1([],[],C,Shot) -> {ok,C};
ok1([],Y,C,Shot)  -> kvs:info(?MODULE,"Error Y: ~p Left ~p Count ~p~n",[hd(Y),length(tl(Y)),C]);
ok1(X, [],C,Shot) -> kvs:info(?MODULE,"Error X: ~p Left ~p Count ~p~n",[hd(X),length(tl(X)),C]);
ok1([X|XR]=XF,[Y|YR]=YF,C,Shot)  -> case element(#tick.price,Y) == element(#tick.price,X) of
                              true -> case element(#tick.size,Y) == element(#tick.size,X) of
                                           true   -> ok1(XR,YR,C+1,Shot);
                                           false  -> kvs:info(?MODULE,"Size Mismatch:~n1: ~p~n2: ~p~nTick: ~p~nBook: ~p~n",
                                                     [X,Y,shotlevel(Shot,element(#tick.price,X)),
                                                          pricelevel(element(1,Y),element(#tick.price,X))]),
                                                     ok1(XR,YR,C+1,Shot)
                                       end;
                              false -> case element(#tick.price,Y) > element(#tick.price,X) of
                                            true  -> ok1(XF,YR,C+1,Shot);
                                            false -> ok1(XR,YF,C+1,Shot)
                                       end
                         end.

shotlevel(Shot,Price) -> [ [trade:normal(P),trade:normal(S),I] || [P,S,I] <- Shot, trade:normal(P) == Price ].
pricelevel(Sym,Price) -> [ [P,S,I] || #order{price=P,uid=I,size=S} <- kvs:index(order,sym,Sym), P == Price ].

%  shot:check( pid(0,437,0),'ETH-BTC').
check(Pid,Topic) ->
    mnesia:clear_table(tick),
    Pid ! {right, Topic, self()},
    {A2,B2,Shot} = receive X -> X after 4000 -> {[],[],[]} end,
    shot:ok(A2,B2,Shot).
