-module(book).
-include("core.hrl").
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/config.hrl").
-compile(export_all).

media()    -> kvs:change_storage(tick, ram_copies).
metainfo() ->
    #schema{name = trading, tables = [
          #table{name = tick, fields=record_info(fields, tick)} ]}.

