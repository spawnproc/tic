-module(venue_sup).
-description('Venue Channel Supervisor').
-include("core.hrl").
-compile(export_all).
-record(state, {venue,endpoint,timer=[]}).
-export(?GEN_SERVER).

timer_restart(Diff) ->
    {X,Y,Z} = Diff, erlang:send_after(1000*(1+Z+60*Y+60*60*X),self(),{timer,connect}).

handle_info({timer,connect}, State=#state{endpoint=URL,venue=Venue,timer=Timer}) ->
    kvs:info(?MODULE,"PING STATE: ~p~n",[State]),
    T = case Timer of
                [] -> skip;
                 _ -> try websocket_client:start_link(URL, Venue, []), []
                    catch E:R -> erlang:cancel_timer(Timer), timer_restart({0,0,5}) end end,
    {noreply,State#state{timer=T}};

handle_info({'EXIT', Pid,_}, #state{} = State) -> {noreply, State};
handle_info(_Info, State) -> {noreply, State}.
start_link(Venue,URL) -> gen_server:start_link(?MODULE, [Venue,URL], []).
handle_call(Request,_,Proc) -> {reply,ok,Proc}.
handle_cast(Msg, State) -> {stop, {error, {unknown_cast, Msg}}, State}.
terminate(_Reason, #state{}) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
init([Venue,URL]) -> {ok,#state{venue=Venue,
                                endpoint=URL,
                                timer=erlang:send_after(100,self(),{timer,connect})}}.
