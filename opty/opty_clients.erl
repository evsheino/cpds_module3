-module(opty_clients).
-export([start/8, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Updates: ??
%% Time: Duration of the experiment (in secs)
%% Reads: Number of reads per transaction.
%% Writes: Numbero of writes per transaction.
%% Num_Subset: Size of each clients entry set.
%% Server: Remote id of the server process.
start(Clients, Entries, Updates, Time, Reads, Writes, Num_Subset, Server) ->
    L = startClients(Clients, [], Entries, Updates, Reads, Writes, Num_Subset, Server),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w UPDATES PER TRANSACTION, 
    DURATION ~w s ~n", [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L, Server).

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

stop(L, Server) ->
    io:format("Stopping...~n"),
    stopClients(L),
    Server ! stop.

startClients(0,L,_,_,_,_,_, _) -> L;
startClients(Clients, L, Entries, Updates, Reads, Writes, Num_Subset, Server) ->
    Subset = generate_subset(Num_Subset, Entries, []),
    Pid = client:start(Clients, Entries, Updates, Server, Reads, Writes, Subset),
    startClients(Clients-1, [Pid|L], Entries, Updates, Reads, Writes, Num_Subset, Server).

stopClients([]) -> ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
