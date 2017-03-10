-module(snapshot).
-include("core.hrl").
-compile(export_all).

sync(Venue,Topic)  -> Venue:left_cut(Topic).
check(Venue,Topic) -> Venue:right_cut(Topic).

book(Venue,Topic) -> [ X || X <- kvs:all(Venue:name(Topic)), element(#tick.size,X) /= 0].

sort(B1)          -> lists:sort(fun(X,Y) -> element(#tick.price,X) > element(#tick.price,Y) end,B1).
ok(V,A,B,Shot)      -> ok1(V,sort(B),sort(A),0,Shot,[]).
ok1(V,[],[],C,Shot,A) -> {ok,C,length(A),A};
ok1(bitmex,[],[Y],C,Shot,A)  -> {ok,C,length(A),A};
ok1(V,[],Y,C,Shot,A)  -> kvs:info(?MODULE,"Error Y: ~p Left ~p Count ~p~n",[hd(Y),length(tl(Y)),C]);
ok1(V,X, [],C,Shot,A) -> kvs:info(?MODULE,"Error X: ~p Left ~p Count ~p~n",[hd(X),length(tl(X)),C]);
ok1(V,[X|XR]=XF,[Y|YR]=YF,C,Shot,A)  -> case element(#tick.price,Y) == element(#tick.price,X) of
                              true -> case element(#tick.size,Y) == element(#tick.size,X) of
                                           true   -> ok1(V,XR,YR,C+1,Shot,A);
                                           false  -> Acc = [V:shotlevel(Shot,element(#tick.price,X)),
                                                          pricelevel(element(1,Y),element(#tick.price,X))],
%                                                     kvs:info(?MODULE,"Size Mismatch~nTick: ~p~nBook: ~p~n",Acc),
                                                     ok1(V,XR,YR,C+1,Shot,[Acc|A])
                                       end;
                              false -> case element(#tick.price,Y) > element(#tick.price,X) of
                                            true  -> ok1(V,XF,YR,C+1,Shot,A);
                                            false -> ok1(V,XR,YF,C+1,Shot,A)
                                       end
                         end.

shotlevel(Shot,Price) -> [ [app:normal(P),app:normal(S),I] || [P,S,I] <- Shot, app:normal(P) == Price ].
pricelevel(Sym,Price) -> [ [P,S,I] || #order{price=P,uid=I,size=S} <- kvs:index(order,sym,Sym), P == Price ].

do(V,Topic) ->
    mnesia:clear_table(tick),
    application:get_env(tic,V,[]) ! {right, Topic, self()},
    {A2,B2,Shot} = receive X -> X after 4000 -> {[],[],[]} end,
    kvs:info(?MODULE,"~p~n",[{length(A2),length(B2)}]),
    snapshot:ok(V,A2,B2,Shot).
