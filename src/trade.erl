-module(trade).
-description('TIC Trading Platform').
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1]).

venues() -> [{gdax,   "wss://ws-feed.gdax.com"},
             {bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,instruments"}].

order_trace(Venue,[A,Sym,S,P,Side,Debug,Timestamp,OID]) ->
    {{Y,M,D},_}=calendar:universal_time(),
    file:make_dir(lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D])),
    FileName = lists:concat(["priv/",Venue,"/",Y,"-",M,"-",D,"/",Sym]),
    Order = lists:flatten(sym:f(Timestamp,Venue:order(Sym,A,Side,normal(p(S)),normal(p(P)),Debug,OID))),
    case application:get_env(trade,log,hide) of
         show -> io:format("~p:~p:~p:~p:~s\r",[Venue,Sym,Side,Order,OID]);
            _ -> skip end,
    file:write_file(FileName, list_to_binary(Order), [raw, binary, append, read, write]).

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

print_float(X) ->
    case string:tokens(X,"-+") of
         [S,N] -> lists:concat([S,flo(N)]); [I] -> flo(I) end.

flo(N) -> P = string:right(N,8,$0),
    lists:concat([case string:substr(N,1,erlang:max(length(N)-8,0)) of
                 [] -> "0"; E -> string:strip(E, left, $0) end, ".",
                 case string:substr(P,length(P)-8+1,8) of
                 [] -> "0"; E -> case string:strip(E, right, $0) of
                                 [] -> "0";
                                  B -> B end end]).