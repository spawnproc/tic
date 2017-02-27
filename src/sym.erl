-module(sym).
-behaviour(rest).
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(sym).

instance() -> #sym{}.
post({Data},_) -> from_json(Data, instance()).

f(T,[])         -> [];
f(T,[A,B])      -> io_lib:format("~s:[~p ~s]~n",    [T,A,B]);
f(T,[A,B,C])    -> io_lib:format("~s:[~p ~s ~s]~n", [T,A,B,C]);
f(T,X)          -> io_lib:format("~s:[~p]~n",       [T,X]).

show() -> application:set_env(trade,log,show).
hide() -> application:set_env(trade,log,hide).
