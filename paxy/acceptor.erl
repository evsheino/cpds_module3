-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),
  Promised = order:null(), 
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(..., ...) of
        true ->
          ... ! {promise, ..., ..., ...},               
              % Update gui
              if
                Value == na ->
                  Colour = {0,0,0};
                true ->
                  Colour = Accepted
              end,                
              io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                        [Name, ..., ..., Value]),
              PanelId ! {updateAcc, "Round voted: " 
                         ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: " 
                         ++ lists:flatten(io_lib:format("~p", [...])), Colour},
              acceptor(Name, ..., Voted, Value, PanelId);
false ->
                ... ! {sorry, {prepare, ...}},
                    acceptor(Name, ..., Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(..., ...) of
        true ->
          ... ! {vote, ...},
              case order:goe(..., ...) of
                true ->
                  % Update gui
                  io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                            [Name, ..., ..., ...]),
                  PanelId ! {updateAcc, "Round voted: " 
                             ++ lists:flatten(io_lib:format("~p", [...])), "Cur. Promise: " 
                             ++ lists:flatten(io_lib:format("~p", [Promised])), ...},
                  acceptor(Name, Promised, ..., ..., PanelId);
                false ->
                  % Update gui
                  io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                            [Name, ..., ..., ...]),
                  PanelId ! {updateAcc, "Round voted: " 
                             ++ lists:flatten(io_lib:format("~p", [...])), "Cur. Promise: " 
                             ++ lists:flatten(io_lib:format("~p", [Promised])), ...},
                  acceptor(Name, Promised, ..., ..., PanelId)
              end;                            
false ->
                ... ! {sorry, {accept, ...}},
                    acceptor(Name, Promised, ..., ..., PanelId)
      end;
    stop ->
      ok
  end.
