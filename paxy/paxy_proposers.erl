-module(paxy_proposers).
-export([start/2]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep, AccController) ->
    ProposerNames = ["Proposer 1", "Proposer 2", "Proposer 3"],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
    register(control, self()),
    AccController ! {props, {control, node()}, {PropInfo, ProposerNames}},
    receive
        {acc, AccRegister, PropIds} ->
            io:format("AccRegister ~w PropIds ~w ~n",
                        [AccRegister, PropIds]),
            start_proposers(PropIds, PropInfo, AccRegister, Sleep)
    end,
    unregister(control),
    true.

start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId),	
            start_proposers(Rest, RestInfo, Acceptors, RestSleep)
        end.
