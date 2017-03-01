-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

instruments() -> [ N || #table{name=N,keys=[id,price]} <- kvs:tables() ].

free({Sym,UID}) -> kvs:put(#io{id=UID,sym=Sym}).
alloc(Symbol)   -> case kvs:index(io,sym,Symbol) of [] -> kvs:next_id(Symbol,1);
                       [#io{uid=Key,sym=Sym,id=UID}|_] -> kvs:delete(io,Key), UID end.

metainfo() ->
    #schema { name = trading,  tables = [
     #table { name = io,                  fields = record_info(fields, io),   keys=[sym,id]  },
     #table { name = order,               fields = record_info(fields, order) },
     #table { name = bitmex_btc_usd_swap, fields = record_info(fields, tick), keys=[id,price] },
     #table { name = bitmex_coin_future,  fields = record_info(fields, tick), keys=[id,price] },
     #table { name = bitmex_dash_futute,  fields = record_info(fields, tick), keys=[id,price] },
     #table { name = bitmex_eth_future,   fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'gdax_btc_usd',      fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'gdax_btc_eur',      fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'gdax_btc_gbp',      fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'gdax_eth_btc',      fields = record_info(fields, tick), keys=[id,price] },
     #table { name = 'gdax_eth_usd',      fields = record_info(fields, tick), keys=[id,price] }   ] }.

add(#tick{sym=[]}) -> [];

add(#tick{price=P,size=S,sym=Sym,id=O,side=Side}=T) ->
    case kvs:index(Sym,price,P) of
         [{Sym,_,P,Id,XS,Sym,_}=X] ->
               UID=book:alloc(Sym),
               kvs:put(#order{uid=O,local_id=UID,sym=Sym}),
               kvs:put(setelement(#tick.size,X,XS+S)), [UID,P,abs(S),Side];
         [] -> UID=book:alloc(Sym),
               kvs:put(#order{uid=O,local_id=UID,sym=Sym}),
               kvs:put(setelement(1,
                       setelement(#tick.size,
                       setelement(#tick.id,
                       setelement(#tick.uid,T,O),UID),S),Sym)), [UID,P,abs(S),Side] end.

del(#tick{sym=[]}) -> [];

del(#tick{price=[],id=O,size=S,sym=Sym}=Tick) ->
    case kvs:get(order,O) of
         {error,_} -> [];
         {ok,#order{uid=O,local_id=UID}} ->
               case kvs:get(Sym,UID) of
                    {ok,X} -> XS = element(#tick.size,X),
                              kvs:put(setelement(#tick.size,X,XS+S)),
                              book:free({Sym,UID}),
                              kvs:delete(order,O), [UID];
                         _ -> [UID] end end;

del(#tick{price=P,id=O,size=S,sym=Sym}=Tick) ->
    case kvs:index(Sym,price,P) of
         [] -> [];
         [{Sym,_,P,_,XS,Sym,Side}=X] ->
             case kvs:get(order,O) of
                  {error,_} -> [];
                  {ok,#order{uid=O,local_id=UID}} ->
                       kvs:put(setelement(#tick.size,X,XS+S)),
                       book:free({Sym,UID}),
                       kvs:delete(order,O), [UID] end end.

ask(S) -> lists:concat(["\e[38;2;208;002;027m",S,"\e[0m"]).
bid(S) -> lists:concat(["\e[38;2;126;211;033m",S,"\e[0m"]).

print(Book) ->
    F      = fun(X, Y) -> trade:nn(element(#tick.price,X)) < trade:nn(element(#tick.price,Y)) end,
    Sorted = lists:sort(F, kvs:all(Book)),

    {PW,SW} = lists:foldr(fun({_,_,P,_,S,_,_},{X,Y}) ->
                 { erlang:max(X,length(trade:print_float(P))),
                   erlang:max(Y,length(trade:print_float(integer_to_list(S)))) } end, {0,0}, Sorted),

    io:format("~s ~s~n", [string:left("Price",PW,$ ),
                          string:left("Size",SW,$ )]),

    io:format("~s ~s~n", [lists:duplicate(PW,"-"),lists:duplicate(SW,"-")]),

    {Depth,Total}  = lists:foldr(fun({_,_,_,_,0,_,_},A) -> A;
                                    ({_,_,P,_,S,_,_},{D,Acc}) ->

    Side = case S < 0 of true -> ask; _ -> bid end,

    io:fwrite(<<"~s">>,[book:Side(io_lib:format("~s ~s~n",
            [ string:right(trade:print_float(P),PW,$ ),
              string:left(trade:print_float(integer_to_list(S)),SW,$ ) ]))]), {D+1,Acc+S} end, {0,0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[trade:print_float(trade:p(Total))]).
