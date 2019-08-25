-module(book).
-include("core.hrl").
%-include_lib("kvs/include/metainfo.hrl").
%-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

backend()       -> ram_copies.
instruments()   -> []. %[ N || #table{name=N,keys=[id]} <- kvs:tables() ].
free({Sym,UID}) -> kvs:put(#io{uid=UID,id=UID,sym=Sym}).
alloc(Symbol)   -> case kvs:index(io,sym,Symbol) of [] -> kvs:next_id(Symbol,1);
                       [#io{uid=Key,sym=Sym,id=UID}|_] -> kvs:delete(io,Key), UID end.

%metainfo() ->
%    #schema { name = trading,  tables = [
%     #table { name = io,                  fields = record_info(fields, io),   keys=[sym,id],        copy_type=backend() },
%     #table { name = order,               fields = record_info(fields, order),keys=[sym,price,uid], copy_type=backend() },
%     #table { name = tick,                fields = record_info(fields, tick), keys=[sym,id],        copy_type=backend() },
%     #table { name = bitmex_btc_usd_swap, fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = bitmex_coin_future,  fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = bitmex_dash_future,  fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = bitmex_eth_future,   fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = 'gdax_btc_usd',      fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = 'gdax_btc_eur',      fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = 'gdax_btc_gbp',      fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = 'gdax_eth_btc',      fields = record_info(fields, tick), keys=[id], copy_type=backend() },
%     #table { name = 'gdax_eth_usd',      fields = record_info(fields, tick), keys=[id], copy_type=backend() }   ] }.

add(#tick{sym=[]}) -> [];
add(#tick{price=P,size=S,sym=Sym,id=O,side=Side,sn=Q}=T) ->
    UID = book:alloc(Sym),
    case Sym of
         tick -> [];
            _ -> kvs:put(#order{uid=O,local_id=UID,sym=Sym,price=P,sn=Q,size=S,side=Side}) end,
    case kvs:get(Sym,P) of
         {ok,{Sym,P,XS,_,Sym,_,_}=X} ->
               kvs:put(setelement(#tick.size,X,XS+abs(S))),
               [UID,P,abs(S),Side];
         {error,_} -> kvs:put(setelement(1,
                       setelement(#tick.size,T,abs(S)),Sym)),
               [UID,P,abs(S),Side] end.

del(#tick{sym=[]}) -> [];
del(#tick{id=O,sym=Sym}=Tick) ->
    case kvs:index(order,uid,O) of
         [] -> %kvs:info(?MODULE,"Delete Error: ~p~n",[Tick]),
                       [];
         [#order{uid=O,local_id=UID,price=Price,size=S,side=Side,sn=Serial}|_] ->
               book:free({Sym,UID}),
               kvs:delete(order,Serial),
               case kvs:get(Sym,Price) of
                    {ok,X} -> kvs:put(setelement(#tick.size,X,
                                   element(#tick.size,X)-abs(S))),
                              [Price,UID];
                 {error,_} -> [Price,UID] end end.

ask(S) -> lists:concat(["\e[38;2;208;002;027m",S,"\e[0m"]).
bid(S) -> lists:concat(["\e[38;2;126;211;033m",S,"\e[0m"]).

print0(Book) ->
    F      = fun(X, Y) -> tic:nn(element(#tick.price,X)) < tic:nn(element(#tick.price,Y)) end,
    Sorted = lists:sort(F, kvs:all(Book)),

    {PW,SW} = lists:foldr(fun({_,P,S,_,_,_,_},{X,Y}) ->
                 { erlang:max(X,length(tic:print_float(P))),
                   erlang:max(Y,length(tic:print_float(integer_to_list(S)))) } end, {0,0}, Sorted),

    io:format("~s ~s~n", [string:left("Price",PW,$ ),
                          string:left("Size",SW,$ )]),

    io:format("~s ~s~n", [lists:duplicate(PW,"-"),lists:duplicate(SW,"-")]),

    {Depth,Strings,Total}  = lists:foldr(fun({_,_,0,_,_,_,_},A) -> A;
                                    ({_,P,S,_,_,_,_},{D,Strings,Acc}) ->

    Side = case S < 0 of true -> ask; _ -> bid end,

    Str = io_lib:format("~s",[io_lib:format("~s ~s~n",
            [ string:right(tic:print_float(P),PW,$ ),
              string:left(tic:print_float(integer_to_list(S)),SW,$ ) ])]),

    io:fwrite(<<"~s">>,[book:Side(Str)]),

    {D+1,[Str|Strings],Acc+S} end, {0,[],0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[tic:print_float(tic:p(Total))]),

    {Depth,lists:reverse(Strings),Total}.

print(Book) -> {A,B,C} = print0(Book), {A,C}.
