-module(paxy_acceptors).
-export([start/0, stop/0, stop/1, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

start() ->
    register(controller, self()),
    AcceptorNames = ["Acceptor 1", "Acceptor 2", "Acceptor 3", 
    "Acceptor 4", "Acceptor 5"],
    AccRegister = [a, b, c, d, e],
    receive
        %% Proposer information is received from the proposer node.
        {props, Controller, Props} ->
            {PropInfo, ProposerNames} = Props
    end,
    % computing panel heights
    AccPanelHeight = length(AcceptorNames)*50 + 20, %plus the spacer value
    PropPanelHeight = length(ProposerNames)*50 + 20,
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames,
    AccPanelHeight, PropPanelHeight) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister)
    end,
    lists:map(fun(X) -> {{Name, _}, P} = X, register(Name, P) end, lists:zip(PropInfo, PropIds)),
    %% Send acceptor and gui references to the proposer node.
    Controller ! {acc, 
                  lists:map(fun(X) -> {X, node()} end, AccRegister), 
                  lists:map(fun(X) -> {Name, _} = X, {Name, node()} end, PropInfo)},
    true.
    
start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            register(RegName, acceptor:start(RegName, AccId)),
            start_acceptors(Rest, RegNameRest)
    end.

stop() ->
    stop(gui),
    stop(a),
    stop(b),
    stop(c),
    stop(d),
    stop(e).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

crash(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            unregister(Name),
            exit(Pid, "crash"),
            register(Name, acceptor:start(Name, na))
    end.
