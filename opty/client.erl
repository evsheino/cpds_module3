-module(client).
-export([start/7]).

start(Name, Entries, Updates, Server, Reads, Writes, Subset) ->
    spawn(fun() -> open(Name, Entries, Updates, Server, 0, 0, Reads, Writes, Subset) end).

open(Name, Entries, Updates, Server, Total, Ok, Reads, Writes, Subset) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format("~w: Subset used:~w ~n",[Name, Subset]),
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [Name, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            do_transactions(Name, Entries, Updates, Server, Handler,
                            Total, Ok, Updates, Reads, Writes, Subset)
    end.

% Commit transaction
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, 0, Reads, Writes, Subset) ->
    %io:format("~w: Commit: TOTAL ~w, OK ~w~n", [Name, Total, Ok]),
    %timer:sleep(Name*10),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Server, Total+1, Ok+1, Reads, Writes, Subset);
        true ->
            open(Name, Entries, Updates, Server, Total+1, Ok, Reads, Writes, Subset)
    end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N, Reads, Writes, Subset) ->
    %io:format("~w: R/W: TOTAL ~w, OK ~w, N ~w~n", [Name, Total, Ok, N]),
    Ref = make_ref(),
    Index = random:uniform(tuple_size(Subset)),
    Num = element(Index, Subset),
    do_reads(Handler, Ref, Num, Reads),
    if
	Reads == 0 ->
	    Value = 1;
	true ->
	    Value = receiveValue(Ref)
    end,
    do_writes(Handler, Num, Value, Writes),
    do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N-1, Reads, Writes, Subset).

do_reads(_, _, _, 0) ->
    ok;

do_reads(Handler, Ref, Num, N) ->
    Handler ! {read, Ref, Num},
    do_reads(Handler, Ref, Num, N-1).

do_writes(_, _, _, 0) ->
    ok;

do_writes(Handler, Num, Value, N) ->
    Handler ! {write, Num, Value+1},
    do_writes(Handler, Num, Value+1, N-1).


receiveCommitValue(Ref) ->
    receive
        {Ref,Value} -> Value
    end.

receiveValue(Ref) ->
    receive
        {value,Ref,Value} -> Value
    end.
