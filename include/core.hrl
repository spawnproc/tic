
-record(core, { operation, resource, module, req, method, vsn, api_key }).
-record(ctx, { user=[] }).

-define(JSON, (application:get_env(trade,json,jsone))).
