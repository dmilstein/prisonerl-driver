% Prisoner whose play is based on Austen's Darcy.
%
% An opponent who even once defects loses Darcy's good opinion.
%
% Darcy's good opinion, once lost, is lost forever.

-module(darcy).
-export([init/0, play/2, result/2, stop/1]).

-import(lists, [member/2]).

-record(state,
	{good_opinion_lost=[]}).

init() ->
     #state{}.

play(Opponent, State) ->
    GoodOpinionLost = member(Opponent, State#state.good_opinion_lost),
    if 
	GoodOpinionLost ->
	    {defect, State};
	true ->
	    {cooperate, State}
    end.

result({Opponent, _OwnChoice, OpponentChoice}, State) ->
    if
	OpponentChoice =:= defect ->
	    NewGoodOpinionLost = [Opponent | State#state.good_opinion_lost],
	    State#state{good_opinion_lost=NewGoodOpinionLost};
	true ->
	    State
    end.

stop(_State) ->
     ok.
