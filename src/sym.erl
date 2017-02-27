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

f(T,[])         -> [];
f(T,[0,B])      -> [];
f(T,[A,B])      -> io_lib:format("~s:[~p ~s]~n",    [T,A,B]);
f(T,[A,B,C])    -> io_lib:format("~s:[~p ~s ~s]~n", [T,A,B,C]);
f(T,[A,B,C,D])  -> io:format("~p~n", [D]), io_lib:format("~s:[~p ~s ~s]~n", [T,A,B,C]);
f(T,X)          -> io_lib:format("~s:[~p]~n",       [T,X]).

show() -> application:set_env(trade,log,show).
hide() -> application:set_env(trade,log,hide).
