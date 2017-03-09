
-record(io,     { uid=[], id=[], sym=[] }).
-record(order,  { sn=[], uid=[], local_id=[], sym=[], price=[], size=0, side=[] }).
-record(tick,   { price=[], size=0, side=[], sym=[], id=[], sn=[] }).
-record(shot,   { sequence=[], bids=[], asks=[] }).

-define(GEN_SERVER, [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
