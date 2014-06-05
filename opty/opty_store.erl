-module(opty_store).
-export([start/1]).

start(Entries) ->
    register(s, server:start(Entries)).
