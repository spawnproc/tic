-module(bshot).
-behaviour(rest).
-include("core.hrl").
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/1]).
-rest_record(bshot).

get(Tick) ->
    ret(httpc:request(get,
        {lists:concat(["https://www.bitmex.com/api/v1/orderBook/L2?symbol=",Tick,"&depth=0"]),
        [{"User-Agent","ticker"}]},[],[])).

post({D}) -> from_json(D,#bshot{}).

ret({ok,{{_,C,_},_,A}}) when C>=200, C<300 -> [ post(T) || T <- jsone:decode(list_to_binary(A)) ];
ret({ok,{S,_,B}})                          -> #bshot{};
ret(Error)                                 -> #bshot{}.
