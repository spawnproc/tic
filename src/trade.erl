-module(trade).
-description('Erlang Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1]).

tables()   -> [ cookies, actions, globals, caching ].
opt()      -> [ set, named_table, { keypos, 1 }, public ].
start(_,_) -> websocket_client:start_link("wss://www.bitmex.com/realtime", client, []).
stop(_)    -> ok.
init([])   -> [ ets:new(T,opt()) || T <- tables() ],
              { ok, { { one_for_one, 5, 10 }, [] } }.
