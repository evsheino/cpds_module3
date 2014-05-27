-module(acceptor).
-export([start/2]).

-define(delay, 1000).
-define(drop, 5).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  {A1,A2,A3} = now(),
  random:seed(A1, A2, A3),
  %% Make sure that the table exists
  {Promised, Voted, Value, Panel} = pers:read(Name),
  case PanelId of
    na ->
      io:format("Restarting acceptor ~p~n", [Name]),
      acceptor(Name, Promised, Voted, Value, Panel);
    _ ->
      acceptor(Name, order:null(), order:null(), na, PanelId)
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  pers:store(Name, Promised, Voted, Value, PanelId),
  receive
    {prepare, Proposer, Round} ->
      timer:sleep(random:uniform(?delay)),
      case order:gr(Round, Promised) of
        true ->
          send(Proposer, {promise, Round, Voted, Value}),
          % Update gui
          if
            Value == na ->
              Colour = {0,0,0};
            true ->
              Colour = Value
          end,                
          io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                    [Name, Voted, Round, Value]),
          PanelId ! {updateAcc, "Round voted: " 
                     ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: " 
                     ++ lists:flatten(io_lib:format("~p", [Round])), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          send(Proposer, {sorry, {prepare, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;

    {accept, Proposer, Round, Proposal} ->
      timer:sleep(random:uniform(?delay)),
      case order:goe(Round, Promised) of
        true ->
          send(Proposer, {vote, Round}),
          case order:goe(Round, Voted) of
            true ->
              % Update gui
              io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                        [Name, Round, Promised, Proposal]),
              PanelId ! {updateAcc, "Round voted: " 
                         ++ lists:flatten(io_lib:format("~p", [Round])), "Cur. Promise: " 
                         ++ lists:flatten(io_lib:format("~p", [Promised])), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              % Update gui
              io:format("[Acceptor ~w] set gui: voted ~w promised ~w colour ~w~n",
                        [Name, Voted, Promised, Value]),
              PanelId ! {updateAcc, "Round voted: " 
                         ++ lists:flatten(io_lib:format("~p", [Voted])), "Cur. Promise: " 
                         ++ lists:flatten(io_lib:format("~p", [Promised])), Value},
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
          send(Proposer, {sorry, {accept, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      pers:delete(Name),
      ok
  end.

send(Name, Message) ->
  case random:uniform(?drop) of
    ?drop ->
      io:format("message dropped~n");
    _ ->
      Name ! Message
  end.
