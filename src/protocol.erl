-module(protocol).
-description('TIC WebSocket Protocol').
-include_lib("n2o/include/wf.hrl").
-compile(export_all).

finish(State,Ctx) -> {ok,State,Ctx}.
init(State,Ctx)   -> {ok,State,Ctx#cx{module=protocol}}.
event(Event)      -> kvs:info(?MODULE,"Event: ~p~n",[Event]).

info({text,Text}=Message, Req, State) when is_binary(Text) ->
    Path = route(wf:path(Req)),
    Module = State#cx.module,
    wf:info(?MODULE,"TIC Message: ~p ~p ~p ~p~n",[Message,Path,Module,Path]),
    {reply, <<"TIC">>, Req, State};

info(Message, Req, State) -> {unknown,Message, Req, State}.

route(<<>>)                         -> {tic,zero};
route(<<"/gdax/",Symbol/binary>>)   -> {gdax,wf:to_atom(Symbol)};
route(<<"/bitmex/",Symbol/binary>>) -> {bitmex,wf:to_atom(Symbol)};
route(_)                            -> {tic,unknown}.
