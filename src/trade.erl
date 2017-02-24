-module(trade).
-description('Erlang Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, log_modules/0, main/1, order_trace/7]).

venues() -> [{gdax,   "wss://ws-feed.gdax.com"},
             {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,orderBookL2:XBTUSD"}].

order_trace(Venue,A,Sy,S,P,Side,Debug) ->
     {{Y,M,D},_}=calendar:universal_time(),
     file:make_dir(lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D])),
     FileName = lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D,"/",Sy]),
     Order = list_to_binary(sym:f(Venue:order(Sy,A,Side,s(S),S,p(P),Debug))),
     file:write_file(FileName, Order, [raw, binary, append, read, write]).

stop(_)       -> ok.
main(A)       -> mad:main(A).
init([])      -> { ok, { { one_for_one, 5, 10 }, [] } }.
log_modules() -> [ bitmex, gdax ].
dirs()        -> file:make_dir("priv"), [ file:make_dir(lists:concat(["priv/",X])) || X <- log_modules() ].
start(_,_)    -> dirs(), lists:foldl(fun({B,A},_) ->
                            websocket_client:start_link(A, B, []) end, ok, venues()).

s([]) -> 0;
s(X)  -> X.
p(X) when is_integer(X) -> integer_to_list(X);
p(X) when is_float(X)   -> float_to_list(X,[{decimals,9},compact]);
p(X) -> X.
