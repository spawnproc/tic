-module(sym).
-behaviour(rest).
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(sym).

instance() -> #sym{}.
post({Data},_) -> from_json(Data, instance()).

f(T,[])          -> [];
f(T,[A,"-0"])    -> io_lib:format("~s:[~p -0]~n",    [T,A]);
f(T,[trade,P,S]) -> io_lib:format("~s:[~s ~s]~n",    [T,  trade:print_float(P),trade:print_float(S)]);
f(T,[A,P,S])     -> io_lib:format("~s:[~p ~s ~s]~n", [T,A,trade:print_float(P),trade:print_float(S)]);
f(T,X)           -> io_lib:format("~s:[~p]~n",       [T,X]).
