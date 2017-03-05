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
right(Topic,Asks,Bids,Seq)  -> temp(Asks,Bids,Seq),          {book(Topic),book(tick)}.
left(Topic,Asks,Bids,Seq)   -> current(Topic,Asks,Bids,Seq), {book(Topic),[]}.
order(Topic,Side,S,P,O,Seq) -> gdax:order(Topic,"book",side(Side),trade:normal(S),trade:normal(P),[],O,Seq).

book(Topic) -> [ X || X <- kvs:all(gdax:name(Topic)), element(#tick.size,X) /= 0].

current(Topic,Asks,Bids,Seq) ->
    [ [ case kvs:get(order,O) of
             {ok, Ord} -> [];
             {error, _} -> order(Topic,Side,S,P,O,Seq)
        end || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

temp(Asks,Bids,Seq) ->
    [ [ order(tick,Side,S,P,O,Seq) || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ].

cut(Topic, F, G) ->
    #shot{sequence=Seq,bids=Bids,asks=Asks} = shot:get(Topic), Name = gdax:name(Topic),
    kvs:info(?MODULE,"Sequence: ~p~n",[Seq]),
    {Orders,_} = lists:partition(fun(X) -> F(X,Seq) andalso X#order.sym==Name andalso X#order.sn /= [] end,
                   kvs:all(order)),
    kvs:info(?MODULE,"Orders to Delete: ~p on topic ~p~n",[Orders,Topic]),
    Deleted    = lists:map(fun(#order{size=S,price=P,uid=I,side=Side,sym=Sym,sn=Q}) ->
                  book:del(#tick{size=-S,price=P,id=I, side=Side,sym=Sym,sn=Q}) end, Orders),
    G(Topic,Asks,Bids,Seq).

sort_sym(A1) -> lists:sort(fun(X,Y) -> element(#tick.price,X) > element(#tick.price,Y) end,A1).
sort_tic(B1) -> lists:sort(fun(X,Y) -> element(#tick.price,X) > element(#tick.price,Y) end,B1).

ok(A,B)      -> ok1(sort_tic(B), sort_sym(A),0).
ok1([],[],C) -> {ok,C};
ok1([],Y,C)  -> kvs:info(?MODULE,"Error Y: ~p Left ~p Count ~p~n",[hd(Y),length(tl(Y)),C]);
ok1(X, [],C) -> kvs:info(?MODULE,"Error X: ~p Left ~p Count ~p~n",[hd(X),length(tl(X)),C]);
ok1(X, Y,C)  -> case element(#tick.size,hd(X)) == element(#tick.size,hd(Y)) of
                     true  -> ok1(tl(X),tl(Y),C+1);
                     false -> kvs:info(sym,"~n1: ~p~n2: ~p~nError on ~p Left: ~p/~p~n",
                              [hd(X),hd(Y),C,length(X),length(Y)]) end.

%  shot:check( pid(0,429,0),'ETH-BTC').
check(Pid,Topic) ->
  mnesia:clear_table(tick),
  Pid ! {right, Topic, self()},
  {A2,B2} = receive X -> X after 3000 -> {[],[]} end,
  shot:ok(A2,B2).
