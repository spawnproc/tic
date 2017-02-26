-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = trading, tables = [
          #table{name=tick, fields=record_info(fields,tick),keys=[id,price], copy_type=disc_copies } ]}.

add(#tick{price=P,size=S}=T) ->
   case kvs:index(tick,price,P) of
        [] -> UID=kvs:next_id(tick,1), kvs:put(T#tick{uid=UID,price=P,size=S}), UID;
        [#tick{uid=UID,size=XS}=X]  -> kvs:put(X#tick{size=XS+S}), UID end.

remove(#tick{price=P}) ->
   case kvs:index(tick,price,P) of
        [] -> 0;
        [#tick{uid=UID,size=XS}=X]  -> kvs:put(X#tick{size=0}), UID end.

print() ->
    F      = fun(X, Y) -> trade:nn(X#tick.price) < trade:nn(Y#tick.price) end,
    Sorted = lists:sort(F, kvs:all(tick)),

    {PI,PW,SW} = lists:foldr(fun(#tick{size=S,price=P,uid=UID},{I,X,Y}) ->
                 { erlang:max(I,length(integer_to_list(UID))),
                   erlang:max(X,length(P)),
                   erlang:max(Y,length(integer_to_list(S))) } end, {4,0,0}, Sorted),

    io:format("~s ~s ~s~n", [string:right("Id",PI,$ ),
                             string:left("Price.10e8",PW,$ ),
                             string:left("Size.10e8",SW,$ )]),

    io:format("~s ~s ~s~n", ["----",lists:duplicate(PW,"-"),lists:duplicate(SW,"-")]),

    {Depth,Total}  = lists:foldr(fun(#tick{size=0},A) -> A;
                                    (#tick{size=S,price=P,uid=I},{D,Acc}) ->

    io:format("~s ~s ~s~n",
            [ string:right(integer_to_list(I),PI,$ ),
              string:right(P,PW,$ ),
              string:left(integer_to_list(S),SW,$ ) ]), {D+1,Acc+S} end, {0,0}, Sorted),

    io:format("Depth: ~p~n",[Depth]),
    io:format("Total: ~s~n",[trade:p(Total)]).
