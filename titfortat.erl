%%% The rumored-to-be-optimal-ish strategy for prisoners' dilemma

-module(titfortat).
-export([init/0, play/2, result/2, stop/1]).

init() ->
    dict:new().

play(Opponent, State) ->
    case dict:find(Opponent, State) of
        {ok, OpponentLastMove} ->
            {OpponentLastMove, State};
        error ->
            {cooperate, State}
    end.

result({Opponent, _OwnChoice, OpponentChoice}, State) ->
    dict:store(Opponent, OpponentChoice, State).

stop(_State) ->
    ok.

