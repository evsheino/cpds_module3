-module(opty).
-export([start/7, stop/1]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: Number of times a set of reads and writes are performed in a transaction.
%% Time: Duration of the experiment (in secs)
%% Reads: Number of reads in an update.
%% Writes: Number of writes in an update.
%% Num_Subset: Size of each client's entry set.
start(Clients, Entries, Updates, Time, Reads, Writes, Num_Subset) ->
    register(s, server:start(Entries)),
    L = startClients(Clients, [], Entries, Updates, Reads, Writes, Num_Subset),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, 
    DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L).


generate_subset(0, _, Subset) ->
    list_to_tuple(Subset);

generate_subset(Number, Entries, Subset) ->
    Temp = random:uniform(Entries),
    case lists:any(fun(E) -> E == Temp end, Subset) of
	true ->
	    generate_subset(Number, Entries, Subset);
	false ->
	    generate_subset(Number - 1, Entries, [Temp|Subset])
    end.


stop(L) ->
    io:format("Stopping...~n"),
    stopClients(L),
    s ! stop.

startClients(0,L,_,_,_,_,_) -> L;
startClients(Clients, L, Entries, Updates, Reads, Writes, Num_Subset) ->
    Subset = generate_subset(Num_Subset, Entries, []),
    Pid = client:start(Clients, Entries, Updates, s, Reads, Writes, Subset),
    startClients(Clients-1, [Pid|L], Entries, Updates, Reads, Writes, Num_Subset).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
