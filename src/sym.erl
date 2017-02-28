-module(sym).
-behaviour(rest).
-include("bitmex.hrl").
-compile({parse_transform, rest}).
-compile(export_all).
-export([post/2]).
-rest_record(sym).

f(T,[])          -> [];
f(T,[A])         -> io_lib:format("~s -~p 0~n",    [timestamp(),A]);
f(T,[trade,P,S]) -> io_lib:format("~s ~s ~s~n",    [timestamp(),  trade:print_float(P),trade:print_float(S)]);
f(T,[A,P,S,ask]) -> io_lib:format("~s -~p ~s ~s~n",[timestamp(),A,trade:print_float(P),trade:print_float(S)]);
f(T,[A,P,S,bid]) -> io_lib:format("~s +~p ~s ~s~n",[timestamp(),A,trade:print_float(P),trade:print_float(S)]);
f(T,X)           -> io_lib:format("~s ~p~n",       [timestamp(),X]).

pad(I,X)         -> string:left(integer_to_list(I),X,$0).
instance()       -> #sym{}.
post({Data},_)   -> from_json(Data, instance()).
timestamp()      ->
    {Ma,Sa,Micro} = os:timestamp(),
    {_,{A,B,C}} = calendar:seconds_to_daystime(Ma*1000000 + Sa),
    io_lib:format("~s:~s:~s.~s",[pad(A,2),pad(B,2),pad(C,2),pad(Micro div 1000,3)]).
