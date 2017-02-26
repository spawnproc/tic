-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/config.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = trading, tables = [
          #table{name=tick, fields=record_info(fields,tick),keys=[id,price], copy_type=ram_copies } ]}.

add(#tick{price=P,size=S}=T) ->
   case kvs:index(tick,price,P) of
        [] -> UID=kvs:next_id(tick,1),
              kvs:put(T#tick{uid=UID,id=UID,price=P,size=S}),
              UID;
        [#tick{uid=UID,size=XS}=X] ->
              kvs:put(X#tick{size=XS+S}),
              UID end.

remove(#tick{price=P}) ->
   case kvs:index(tick,price,P) of
        [] -> 0;
        [#tick{uid=UID,size=XS}=X] ->
              % kvs:delete(tick,UID),
              kvs:put(X#tick{size=0}),
              UID end.

print() ->
    F = fun(X, Y) -> X#tick.price < Y#tick.price end,
    Sorted = lists:sort(F, ets:tab2list(tick)),
    Total = lists:foldr(fun(#tick{size=0},Acc) -> Acc;
                         (#tick{size=S,price=P,uid=I}=T,Acc) ->
                                io:format("[~s ~s ~s]~n",
            [string:right(integer_to_list(I),4,$ ),
             string:right(P,16,$ ),
             trade:p(S)]), Acc+S end,0, Sorted),
    io:format("Total: ~s~n",[trade:p(Total)]).
