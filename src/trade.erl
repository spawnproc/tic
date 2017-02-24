-module(trade).
-description('Erlang Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-export([start/2, stop/1, init/1, log_modules/0, main/1]).

stop(_)       -> ok.
main(A)       -> mad:main(A).
init([])      -> { ok, { { one_for_one, 5, 10 }, [] } }.
log_modules() -> [ bitmex, gdax ].
dirs()        -> file:make_dir("priv"),
                 [ file:make_dir(lists:concat(["priv/",X])) || X <- log_modules() ].
start(_,_)    -> dirs(), lists:foldl(fun({B,A},_) ->
                            websocket_client:start_link(A, B, []) end, ok, venues()).

venues()      -> [{gdax,   "wss://ws-feed.gdax.com"},
                  {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,orderBookL2:XBTUSD"}].
