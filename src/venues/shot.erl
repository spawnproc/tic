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

cut() -> cut(gdax,'BTC-USD').
cut(Venue,Topic) -> #shot{sequence=Seq} = shot:get(Topic), Name = Venue:name(Topic),
            {A,B} = lists:partition(fun(X) -> X#order.sn < Seq end,
                      lists:sort(fun(L,R) -> L#order.sn < R#order.sn end,
                      element(1,lists:partition(fun(#order{sym=Name}) -> true; (_) -> false end,
                      kvs:all(order))))),
            {length(A),length(B),A}.
