-module(app).
-description('TIC Trade Integration Platform').
-behaviour(supervisor).
-behaviour(application).
-compile(export_all).
-export([start/2, stop/1, init/1, log_modules/0]).

venues() -> [{bitmex, "wss://www.bitmex.com/realtime?subscribe=trade,orderBookL2"},
             {gdax,   "wss://ws-feed.gdax.com"}].

f(T,[])              -> [];
f(T,[P,A])           -> io_lib:format("~s -~p 0~n",    [timestamp(),A]);
f(T,[trade,P,S,bid]) -> io_lib:format("~s ~s ~s~n",    [timestamp(),  app:print_float(P),app:print_float(S)]);
f(T,[trade,P,S,ask]) -> io_lib:format("~s ~s -~s~n",   [timestamp(),  app:print_float(P),app:print_float(S)]);
f(T,[A,P,S,ask])     -> io_lib:format("~s -~p ~s ~s~n",[timestamp(),A,app:print_float(P),app:print_float(S)]);
f(T,[A,P,S,bid])     -> io_lib:format("~s +~p ~s ~s~n",[timestamp(),A,app:print_float(P),app:print_float(S)]);
f(T,X)               -> io_lib:format("~s ~p~n",       [timestamp(),X]).

trace(Venue,[Stream,A,Sym,S,P,Side,Debug,Timestamp,OID,Seq]) ->
    {{Y,M,D},_}=calendar:universal_time(),
    file:make_dir(lists:concat(["priv/",Venue,"/",Stream,"/",Y,"-",M,"-",D])),
    FileName    = lists:concat(["priv/",Venue,"/",Stream,"/",Y,"-",M,"-",D,"/",Sym]),
    Order = lists:flatten(f(Timestamp,Venue:Stream(Sym,A,Side,normal(p(S)),normal(p(P)),Debug,OID,Seq))),
    case application:get_env(trade,log,hide) of
         show -> kvs:info(?MODULE,"~p:~p:~p:~s ~p~n",[Venue,Sym,Side,Order,Debug]);
            _ -> skip end,
    file:write_file(FileName, list_to_binary(Order), [raw, binary, append, read, write]).

log_modules() -> [ bitmex, gdax, book, bsym, bshot, gshot, boot, snapshot, venue ].
init([])      -> { ok, { { one_for_one, 5, 1 }, [ ws(A,B) || {A,B} <- venues() ] } }.
start(_,_)    -> dirs(), kvs:join(), supervisor:start_link({local,ticker},?MODULE,[]).
stop(_)       -> ok.
precision()   -> 8.
pad(I,X)      -> string:right(integer_to_list(I),X,$0).
ws(Venue,URL) -> {Venue,{venue,start_link,[Venue,URL]},permanent,1000,worker,[Venue]}.
dirs()        -> file:make_dir("priv"),
                 [ begin file:make_dir(lists:concat(["priv/",X])),
                   [ file:make_dir(lists:concat(["priv/",X,"/",Y]))
                   || Y <- [trade,order] ]
                 end || X <- [bitmex, gdax] ].

p(X) when is_integer(X) -> integer_to_list(X);
p(X) when is_float(X)   -> float_to_list(X,[{decimals,8},compact]);
p(X)                    -> X.

timestamp()   ->
    {Ma,Sa,Micro} = os:timestamp(),
    {_,{A,B,C}} = calendar:seconds_to_daystime(Ma*1000000 + Sa),
    io_lib:format("~s:~s:~s.~s",[pad(A,2),pad(B,2),pad(C,2),pad(Micro div 1000,3)]).

n([Z,Y])      -> lists:concat([Z,Y,lists:duplicate(precision() - length(Y),"0")]).
normal(Price) -> lists:flatten(c(string:tokens(Price,"."))).

main(A)-> mad:main(A).
nn([]) -> 0;
nn(X)  -> try list_to_integer(X) catch _:_ -> list_to_float(X) end.
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
              I when is_list(I) -> flo(I);
              I -> flo(p(I)) end.
