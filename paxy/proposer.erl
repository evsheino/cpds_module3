-module(proposer).
-export([start/5]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

init(Name, Proposal, Acceptors, Sleep, PanelId) ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    timer:sleep(Sleep),
    Round = order:null(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    % Update gui
    io:format("[Proposer ~w] set gui: Round ~w Proposal ~w~n",
              [Name, Round, Proposal]),
    PanelId ! {updateProp, "Round: " 
               ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
               ++ lists:flatten(io_lib:format("~p", [Proposal])), Proposal},
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w decided ~w in round ~w~n", 
                      [Name, Acceptors, Decision, Round]),
            {ok, Decision};
        abort ->
            timer:sleep(random:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    Max = order:null(),
    case collect(Quorum, Round, Max, Proposal, Quorum) of
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w] set gui: Round ~w Proposal ~w~n", 
                      [Name, Round, Value]),
            PanelId ! {updateProp, "Round: " 
                       ++ lists:flatten(io_lib:format("~p", [Round])), "Proposal: "
                       ++ lists:flatten(io_lib:format("~p", [Value])), Value},
            accept(Round, Value, Acceptors),
            case vote(Quorum, Round, Quorum) of
                ok ->
                    {ok, Value};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.

collect(0, _, _, Proposal, _) ->
    {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal, MaxSorries) ->
    receive 
        {promise, Round, _, na} ->
            collect(N-1, Round, MaxVoted, Proposal, MaxSorries);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value, MaxSorries);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal, MaxSorries)
            end;
        {promise, _, _,  _} ->
            collect(N, Round, MaxVoted, Proposal, MaxSorries);
        {sorry, {prepare, Round}} ->
            M = MaxSorries-1,
            if M == 0 ->
                io:format("Proposer ~p aborting collect due to amount of sorries~n", [self()]),
                abort;
            true ->
                collect(N, Round, MaxVoted, Proposal, M)
            end;
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal, MaxSorries)
    after ?timeout ->
              abort
    end.

vote(0, _, _) ->
    ok;
vote(N, Round, MaxSorries) ->
    receive
        {vote, Round} ->
            vote(N-1, Round, MaxSorries);
        {vote, _} ->
            vote(N, Round, MaxSorries);
        {sorry, {accept, Round}} ->
            M = MaxSorries-1,
            if M == 0 ->
                io:format("Proposer ~p aborting vote due to amount of sorries~n", [self()]),
                abort;
            true ->
                vote(N, Round, M)
            end;
        {sorry, _} ->
            vote(N, Round, MaxSorries)
    after ?timeout ->
              abort
    end.

prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) -> 
                  send(Acceptor, {prepare, self(), Round}) 
          end,
    lists:map(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) -> 
                  send(Acceptor, {accept, self(), Round, Proposal}) 
          end,
    lists:map(Fun, Acceptors).

send(Name, Message) ->
    case whereis(Name) of
        undefined ->
            down;
        Pid ->
            Pid ! Message
    end.
