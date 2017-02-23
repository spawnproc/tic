-module(trade).
-description('Erlang Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1]).

start(_,_) -> lists:foldl(fun({B,A},_) -> websocket_client:start_link(A, B, []) end, ignore, venues()).
stop(_)    -> ok.
init([])   -> { ok, { { one_for_one, 5, 10 }, [] } }.
venues()   -> [{gdax,   "wss://ws-feed.gdax.com"},
               {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,orderBookL2:XBTUSD"}].
