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

cut() -> cut(gdax,"ETH-USD").
cut(Venue,Topic) ->
    #shot{sequence=Seq,bids=Bids,asks=Asks} = shot:get(Topic), Name = Venue:name(Topic),
    {Orders,_} = lists:partition(fun(X) -> X#order.sn =< Seq
                                   andalso X#order.sym == Name end, kvs:all(order)),
    Ticks  = lists:map(fun(#order{size=S,price=P,uid=I,side=Side,sym=Sym,sn=Q}=O) ->
                            #tick{size=S,price=P,id=I,side=Side,sym=Sym,sn=Q} end, Orders),
    [ T ||  T <- Ticks ], % TODO INVERT TICKS in OrderBook
    [ [ case kvs:get(order,O) of
             {ok, Ord} -> kvs:info(?MODULE,"Skip Added Order: ~p. Check: size ~p at ~p price.~n",[Ord,S,P]);
             {error, _} -> Size = case Side of bid -> trade:nn(trade:normal(S));
                                               ask -> -trade:nn(trade:normal(S)) end,
                           Tick = #tick{size=Size,price=trade:normal(P),sym=Name,side=Side,id=O},
                           book:add(Tick),
                           kvs:info(?MODULE,"Added to Order Book ~p~n",[Tick])
        end || [P,S,O] <- Source ] || {Side,Source} <- [{ask,Asks},{bid,Bids}] ],
    {Ticks,length(Ticks),Asks,Bids,Seq}, Seq.
