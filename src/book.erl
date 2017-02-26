-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = trading, tables = [
          #table{name=tick, fields=record_info(fields,tick),keys=[id,price], copy_type=ram_copies } ]}.

add(#tick{price=P,size=S}=T) ->
   case kvs:index(tick,price,P) of
        [] -> UID=kvs:next_id(tick,1), kvs:put(T#tick{uid=UID,price=P,size=S}), UID;
        [#tick{uid=UID,size=XS}=X]  -> kvs:put(X#tick{size=XS+S}), UID end.

remove(#tick{price=P}) ->
   case kvs:index(tick,price,P) of
        [] -> 0;
        [#tick{uid=UID,size=XS}=X]  -> kvs:put(X#tick{size=0}), UID end.

print() ->
    F      = fun(X, Y) -> X#tick.price < Y#tick.price end,
    Sorted = lists:sort(F, kvs:all(tick)),
    {Depth,Total}  = lists:foldr(fun(#tick{size=0},{D,T}) -> {D,T};
                                    (#tick{size=S,price=P,uid=I},{D,Acc}) ->

    io:format("[~s ~s ~s]~n",
            [ string:right(integer_to_list(I),4,$ ),
              string:right(P,16,$ ),
              trade:p(S) ]), {D+1,Acc+S} end, {0,0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[trade:p(Total)]).
