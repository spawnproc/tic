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
    #shot{sequence=Seq} = shot:get(Topic), Name = Venue:name(Topic),
    {Orders,_} = lists:partition(fun(X) -> X#order.sn =< Seq
                                   andalso X#order.sym == Name end, kvs:all(order)),
    Ticks  = lists:map(fun(#order{size=S,price=P,uid=I,side=Side,sym=Sym}=O) ->
                            #tick{size=-S,price=P,id=I,side=Side,sym=Sym} end, Orders),
    {Ticks,length(Ticks)}.
