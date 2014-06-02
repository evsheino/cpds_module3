-module(client).
-export([start/6]).

start(Name, Entries, Updates, Server, Reads, Writes) ->
    spawn(fun() -> open(Name, Entries, Updates, Server, 0, 0, Reads, Writes) end).

open(Name, Entries, Updates, Server, Total, Ok, Reads, Writes) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    Server ! {open, self()},
    receive
        {stop, From} ->
            io:format("~w: Transactions TOTAL:~w, OK:~w, -> ~w % ~n",
            [Name, Total, Ok, 100*Ok/Total]),
            From ! {done, self()},
            ok;
        {transaction, Validator, Store} ->
            Handler = handler:start(self(), Validator, Store),
            do_transactions(Name, Entries, Updates, Server, Handler,
                            Total, Ok, Updates, Reads, Writes)
    end.

% Commit transaction
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, 0, Reads, Writes) ->
    %io:format("~w: Commit: TOTAL ~w, OK ~w~n", [Name, Total, Ok]),
    %timer:sleep(Name*10),
    Ref = make_ref(),
    Handler ! {commit, Ref},
    Result = receiveCommitValue(Ref),
    if
        Result == ok ->
            open(Name, Entries, Updates, Server, Total+1, Ok+1, Reads, Writes);
        true ->
            open(Name, Entries, Updates, Server, Total+1, Ok, Reads, Writes)
    end;

% Reads and Writes
do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N, Reads, Writes) ->
    %io:format("~w: R/W: TOTAL ~w, OK ~w, N ~w~n", [Name, Total, Ok, N]),
    Ref = make_ref(),
    Num = random:uniform(Entries),
    do_reads(Handler, Ref, Num, Reads),
%    Handler ! {read, Ref, Num},
    Value = receiveValue(Ref),
    do_writes(Handler, Num, Value, Writes),
%    Handler ! {write, Num, Value+1},
    do_transactions(Name, Entries, Updates, Server, Handler, Total, Ok, N-1, Reads, Writes).

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
