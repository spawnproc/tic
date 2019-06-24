-module(protocol).
-description('TIC WebSocket Protocol').
-include_lib("n2o/include/n2o.hrl").
-compile(export_all).

finish(State,Ctx) -> {ok,State,Ctx}.
init(State,#cx{req=Req}=Ctx) -> #{path:=Path}=Req, n2o:reg(route(Path)), {ok,State,Ctx#cx{module=protocol}}.
event(Event) -> ok.

info({text,<<"book">>}=Message, Req, State) ->
    {X,Y} = route(wf:path(Req)),
    {A,B,C} = book:print0(X:name(Y)),
    {reply, iolist_to_binary(B), Req, State};

info({text,Text}=Message, Req, State) -> {reply, Text, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

route(<<>>)                         -> {tic,zero};
route(<<"/gdax/",Symbol/binary>>)   -> {gdax,wf:to_atom(Symbol)};
route(<<"/bitmex/",Symbol/binary>>) -> {bitmex,wf:to_atom(Symbol)};
route(_)                            -> {tic,unknown}.
