-module(trade).
-description('TIC Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1]).

venues() -> [{gdax,   "wss://ws-feed.gdax.com"},
             {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,instruments"}].

trace(Venue,[Stream,A,Sym,S,P,Side,Debug,Timestamp,OID]) ->
    {{Y,M,D},_}=calendar:universal_time(),
    file:make_dir(lists:concat(["priv/",Venue,"/",Stream,"/",Y,"-",M,"-",D])),
    FileName    = lists:concat(["priv/",Venue,"/",Stream,"/",Y,"-",M,"-",D,"/",Sym]),
    Order = lists:flatten(sym:f(Timestamp,Venue:Stream(Sym,A,Side,normal(p(S)),normal(p(P)),Debug,OID))),
    case application:get_env(trade,log,hide) of
         show -> io:format("~p:~p:~p:~s",[Venue,Sym,Side,Order]);
            _ -> skip end,
    file:write_file(FileName, list_to_binary(Order), [raw, binary, append, read, write]).

log_modules() -> [ bitmex, gdax ].
init([])      -> { ok, { { one_for_one, 60, 10 }, [ ws(A,B) || {A,B} <- venues() ] } }.
start(_,_)    -> dirs(), kvs:join(), supervisor:start_link({local,ticker},?MODULE,[]).
stop(_)       -> ok.
precision()   -> 8.
ws(Venue,URL) -> {Venue,{websocket_client,start_link,[URL,Venue,[]]},permanent,1000,worker,[websocket_client]}.
dirs()        -> file:make_dir("priv"),
                 [ begin file:make_dir(lists:concat(["priv/",X])),
                   [ file:make_dir(lists:concat(["priv/",X,"/",Y]))
                   || Y <- [trade,order] ]
                 end || X <- log_modules() ].

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

flo(N) -> P = string:right(N,precision(),$0),
    lists:concat([case string:substr(N,1,erlang:max(length(N)-precision(),0)) of
                 [] -> "0"; E -> case string:strip(E, left, $0) of
                                 [] -> "0"; B -> B end end, ".",
                 case string:substr(P,length(P)-precision()+1,precision()) of
                 [] -> "0"; E -> case string:strip(E, right, $0) of
                                 [] -> "0"; B -> B end end]).

print_float(X) ->
    case X of
         "+"++N -> lists:concat(["+",flo(N)]);
         "-"++N -> lists:concat(["-",flo(N)]);
              I -> flo(I) end.
