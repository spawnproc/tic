
-record(io,     { uid=[], id=[], sym=[] }).
-record(order,  { uid=[], local_id=[], sym=[] }).
-record(tick,   { uid=[], price=[], id=[], size=[], sym=[], side=[] }).

-define(GEN_SERVER, [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
