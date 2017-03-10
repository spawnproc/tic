-module(bsym).
-behaviour(rest).
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(bsym).

instance()       -> #bsym{}.
post({Data},_)   -> from_json(Data, instance()).
