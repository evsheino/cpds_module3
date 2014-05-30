-module(opty).
-export([start/6, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)
start(Clients, Entries, Updates, Time, Reads, Writes) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Reads, Writes),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, 
    DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L).

stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients(0,L,_,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, Reads, Writes) ->
    Pid = client:start(Clients, Entries, Updates, s, Reads, Writes),
    startClients(Clients-1, [Pid|L], Entries, Updates, Reads, Writes).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
