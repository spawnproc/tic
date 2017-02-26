-module(trade).
-description('Erlang Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1]).

venues() -> [{gdax,   "wss://ws-feed.gdax.com"},
             {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,instruments"}].

order_trace(Venue,[A,Sym,S,P,Side,Debug,Timestamp]) ->
    {{Y,M,D},_}=calendar:universal_time(),
    file:make_dir(lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D])),
    FileName = lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D,"/",Sym]),
    %io:format("~p:\r",[[Sym,A,Side,S,normal(p(P)),Debug]]),
    Order = list_to_binary(sym:f(Timestamp,Venue:order(Sym,A,Side,normal(p(S)),normal(p(P)),Debug))),
    %io:format("~p~n\r",[Order]),
    file:write_file(FileName, Order, [raw, binary, append, read, write]).

precision()   -> 8.
log_modules() -> [ bitmex, gdax ].
init([])      -> { ok, { { one_for_one, 60, 10 }, [ ws(A,B) || {A,B} <- venues() ] } }.
ws(Venue,URL) -> {Venue,{websocket_client,start_link,[URL,Venue,[]]},permanent,1000,worker,[websocket_client]}.
dirs()        -> file:make_dir("priv"), [ file:make_dir(lists:concat(["priv/",X])) || X <- log_modules() ].
start(_,_)    -> dirs(), kvs:join(), supervisor:start_link({local,ticker},?MODULE,[]).
stop(_)       -> ok.

p(X) when is_integer(X) -> integer_to_list(X);
p(X) when is_float(X)   -> float_to_list(X,[{decimals,8},compact]);
p(X)                    -> X.

n([Z,Y])      -> lists:concat([Z,Y,lists:duplicate(precision() - length(Y),"0")]).
normal(Price) -> lists:flatten(c(string:tokens(Price,"."))).

nn([]) -> 0;
nn(X)  -> list_to_integer(X).
c([])  -> [];
c([X]) -> n([X,[]]);
c(X)   -> n(X).
