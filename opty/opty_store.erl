-module(opty_store).
-export([start/1]).

%% Entries: Number of entries in the store.
start(Entries) ->
    register(s, server:start(Entries)).
