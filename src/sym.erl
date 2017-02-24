-module(sym).
-behaviour(rest).
-include("bitmex.hrl").
-include("core.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(sym).

instance() -> #sym{}.
post({Data},#ctx{user=User}) -> from_json(Data, instance()).

f([A,B])     -> io_lib:format("[~p ~s]~n",    [A,B]);
f([A,B,C])   -> io_lib:format("[~p ~s ~s]~n", [A,B,C]);
f([A,B,C,D]) -> io_lib:format("[~p ~s ~s]~n", [A,B,C]), io:format("~p~n", [D]);
f(X)         -> io_lib:format("[~p]~n",       [X]).
