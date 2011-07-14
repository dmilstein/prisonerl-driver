%%% An example of an Erlang module to participate in the Prisoner's Dilemma
%%% tournament.
%%%
%%% This chooses randomly ('psycho').

% My Erlang is a bit rusty, not sure if this is the perfect code to copy,
% just fyi and all

-module(psycho).
-export([init/0, play/2, result/2, stop/1]).

init() ->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    []. % Returning State, which is just an empty list for now

play(_Opponent, _State) ->
    R = random:uniform(),
    {if 
         R > 0.5 -> cooperate;
         true -> defect
     end, []}.

result({_Opponent, _OwnChoice, _OpponentChoice}, _State) ->
    [].

stop(_State) ->
    ok.



