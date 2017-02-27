-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

instruments() -> [ N || #table{name=N,keys=[id,price]} <- kvs:tables() ].

metainfo() ->
    #schema { name = trading, tables = [
     #table { name = 'btc_usd', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'btc_eur', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'btc_gpb', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'eth_btc', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'eth_usd', fields = record_info(fields, tick), keys=[id,price] }   ] }.

add(#tick{sym=[]}) -> 0;
add(#tick{price=P,size=S,sym=Sym}=T) ->
   case kvs:index(Sym,price,P) of
        [{Sym,UID,Time,Price,Id,XS,Side,Sym}=X] -> kvs:put(setelement(6,X,XS+S)), UID;
        [] -> UID=kvs:next_id(Sym,1),
              kvs:put(setelement(6,setelement(1,setelement(2,T,UID),Sym),S)), UID end.

del(#tick{sym=[],id=A}) -> 0;
del(#tick{price=P,id=A,sym=Sym}) ->
   case kvs:index(Sym,price,P) of
        [] -> 0;
        [{Sym,UID,Time,Price,Id,XS,Side,Sym}=X] -> kvs:put(setelement(6,X,0)), UID end.

print(Book) ->
    F      = fun(X, Y) -> trade:nn(element(4,X)) < trade:nn(element(4,Y)) end,
    Sorted = lists:sort(F, kvs:all(Book)),

    {PI,PW,SW} = lists:foldr(fun({_,UID,_,P,_,S,_,_},{I,X,Y}) ->
                 { erlang:max(I,length(integer_to_list(UID))),
                   erlang:max(X,length(P)),
                   erlang:max(Y,length(integer_to_list(S))) } end, {4,0,0}, Sorted),

    io:format("~s ~s ~s~n", [string:right("Id",PI,$ ),
                             string:left("Price",PW,$ ),
                             string:left("Size",SW,$ )]),

    io:format("~s ~s ~s~n", ["----",lists:duplicate(PW,"-"),lists:duplicate(SW,"-")]),

    {Depth,Total}  = lists:foldr(fun({_,_,_,_,_,0,_,_},A) -> A;
                                    ({_,I,_,P,_,S,_,_},{D,Acc}) ->

    io:format("~s ~s ~s~n",
            [ string:right(integer_to_list(I),PI,$ ),
              string:right(P,PW,$ ),
              string:left(integer_to_list(S),SW,$ ) ]), {D+1,Acc+S} end, {0,0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[trade:p(Total)]).
