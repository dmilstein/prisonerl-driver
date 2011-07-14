%%% An example of an Erlang module to participate in the Prisoner's Dilemma
%%% tournament.
%%%
%%% This always cooperates ('pushover').

% My Erlang is a bit rusty, not sure if this is the perfect code to copy,
% just fyi and all

-module(pushover).
-export([init/0, play/2, result/2, stop/1]).

init() ->
    []. % Returning State, which is just an empty list for now

play(Opponent, _State) ->
    {cooperate, []}.

result({_Opponent, _OwnChoice, _OpponentChoice}, _State) ->
    [].

stop(_State) ->
    ok.



