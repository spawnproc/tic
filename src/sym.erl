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
f(T,[A,"-0"])   -> io_lib:format("~s:[~p -0]~n",    [T,A]);
f(T,[A,B])      -> io_lib:format("~s:[~p ~s]~n",    [T,A,trade:print_float(B)]);
f(T,[A,B,C])    -> io_lib:format("~s:[~p ~s ~s]~n", [T,A,trade:print_float(B),trade:print_float(C)]);
f(T,X)          -> io_lib:format("~s:[~p]~n",       [T,X]).

show() -> application:set_env(trade,log,show).
hide() -> application:set_env(trade,log,hide).
