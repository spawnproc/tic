-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

instruments() -> [ N || #table{name=N,keys=[id,price]} <- kvs:tables() ].

metainfo() ->
    #schema { name = trading, tables = [
     #table { name = order,    fields = record_info(fields, order) },
     #table { name = 'btc_usd', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'btc_eur', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'btc_gpb', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'eth_btc', fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'eth_usd', fields = record_info(fields, tick), keys=[id,price] }   ] }.

add(#tick{sym=[]}) -> [];

add(#tick{price=P,size=S,sym=Sym,id=O,side=Side}=T) ->
   case kvs:index(Sym,price,P) of
        [{Sym,UID,P,Id,XS,Sym,_}=X] ->
              kvs:put(setelement(#tick.size,X,XS+S)),
              kvs:put(#order{uid=O,local_id=UID,sym=Sym}), [UID,P,S,Side];
        [] -> UID=kvs:next_id(Sym,1),
              kvs:put(setelement(1,
                      setelement(#tick.size,
                      setelement(#tick.uid,T,UID),S),Sym)), [UID,P,S,Side] end.

del(#tick{sym=[]}) -> [];

del(#tick{price=[],id=O,size=S,sym=Sym}) ->
   case kvs:get(order,O) of
        {error,_} -> [];
        {ok,#order{uid=O,local_id=UID}} ->
              case kvs:get(Sym,UID) of
                   {ok,X} -> XS = element(#tick.size,X),
                             kvs:put(setelement(#tick.size,X,XS-S)),
                             kvs:delete(order,O), [UID];
                        _ -> [UID] end end;

del(#tick{price=P,id=O,size=S,sym=Sym}) ->
   case kvs:index(Sym,price,P) of
        [] -> [];
        [{Sym,UID,P,Id,XS,Sym,_}=X] ->
              kvs:put(setelement(#tick.size,X,XS-S)),
              kvs:delete(order,O), [UID] end.

ask(S) -> lists:concat(["\e[38;2;208;002;027m",S,"\e[0m"]).
bid(S) -> lists:concat(["\e[38;2;126;211;033m",S,"\e[0m"]).

print(Book) ->
    F      = fun(X, Y) -> trade:nn(element(#tick.price,X)) < trade:nn(element(#tick.price,Y)) end,
    Sorted = lists:sort(F, kvs:all(Book)),

    {PI,PW,SW} = lists:foldr(fun({_,UID,P,O,S,_,_},{I,X,Y}) ->
                 { erlang:max(I,length(integer_to_list(UID))),
                   erlang:max(X,length(trade:print_float(P))),
                   erlang:max(Y,length(trade:print_float(integer_to_list(S)))) } end, {4,0,0}, Sorted),

    io:format("~s ~s ~s~n", [string:right("Id",PI,$ ),
                             string:left("Price",PW,$ ),
                             string:left("Size",SW,$ )]),

    io:format("~s ~s ~s~n", ["----",lists:duplicate(PW,"-"),lists:duplicate(SW,"-")]),

    {Depth,Total}  = lists:foldr(fun({_,_,_,_,0,_,_},A) -> A;
                                    ({_,I,P,O,S,_,Side},{D,Acc}) ->

    io:fwrite(<<"~s">>,[book:Side(io_lib:format("~s ~s ~s~n",
            [ string:right(integer_to_list(I),PI,$ ),
              string:right(trade:print_float(P),PW,$ ),
              string:left(trade:print_float(integer_to_list(S)),SW,$ ) ]))]), {D+1,Acc+S} end, {0,0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[trade:print_float(trade:p(Total))]).
