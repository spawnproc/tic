-module(venue_sup).
-description('Venue Channel Supervisor').
-include("core.hrl").
-compile(export_all).
-record(state, {venue,endpoint,timer=[]}).
-export(?GEN_SERVER).

timer({X,Y,Z},Timer) when Z > 60 -> timer_restart({X,Y,60},Timer);
timer({X,Y,Z},Timer)             -> timer_restart({X,Y,Z*2},Timer).

timer_restart({X,Y,Z},Timer) ->
    erlang:cancel_timer(Timer),
    erlang:send_after(1000*(1+Z+60*Y+60*60*X),self(),{timer,connect,Z}).

handle_info({timer,connect,Z}, State=#state{endpoint=URL,venue=Venue,timer=Timer}) ->
    kvs:info(?MODULE,"~p ~p~n",[Z,State]),
    T = try case websocket_client:start_link(URL, Venue, [self()]) of
                 {ok,_} -> [ kvs:info(?MODULE,"~p snapshot Symbol: ~p Bids: ~p Asks: ~p~n",
                           [Venue,Symbol,length((Venue:snapshot(Symbol))#shot.bids),
                                         length((Venue:snapshot(Symbol))#shot.asks) ])
                              || Symbol <- [] ], []; %Venue:subscription() ], [];
                 {error,_} -> timer({0,0,Z},Timer) end
    catch E:R -> timer({0,0,Z},Timer) end,
    {noreply,State#state{timer=T}};

handle_info(_Info, State) -> {noreply, State}.
start_link(Venue,URL) -> gen_server:start_link(?MODULE, [Venue,URL], []).
handle_call(Request,_,Proc) -> {reply,ok,Proc}.
handle_cast(Msg, State) -> {stop, {error, {unknown_cast, Msg}}, State}.
terminate(_Reason, #state{}) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
init([Venue,URL]) -> {ok,#state{venue=Venue,
                                endpoint=URL,
                                timer=erlang:send_after(100,self(),{timer,connect,5})}}.
