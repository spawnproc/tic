-module(sym).
-behaviour(rest).
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(sym).

instance()       -> #sym{}.
post({Data},_)   -> from_json(Data, instance()).
