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

f([])        -> [];
f([A,B])     -> io_lib:format("[~p ~s]~n",    [A,B]);
f([A,B,C])   -> io_lib:format("[~p ~s ~s]~n", [A,B,C]);
f([A,B,C,D]) -> io:format("~p~n", [D]), io_lib:format("[~p ~s ~s]~n", [A,B,C]);
f(X)         -> io_lib:format("[~p]~n",       [X]).
