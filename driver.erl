%%% Runs a prisoner's dilemma tournament among a series of strategies,
%%% implemented as modules.
-module(driver).

%-export([run_tournament/0]).
-compile(export_all).

%% Scores
%%  cooperate, cooperate 8, 8
%%  defect, cooperate 10, 0
%%  cooperate, defect 0, 10
%%  defect, defect    4, 4

-record(player,
        {name, process, score=0, history=[]}).

create_players() ->
    Players = [ {"pushover", pushover},
                {"psycho", psycho},
                {"darcy",    darcy} ],
    lists:map(fun({Name, Module}) ->
                      #player{name=Name,
                              process=start_player(Module)}
              end,
              Players).
                                  
%% Run a tournament among a list of { Name, Module } pairs, print the
%% results out.
run_tournament() ->
    N = 10,
    Players = create_players(),
    AllPairs = all_pairs(Players),
    AllScores = lists:map(
      fun({Player1, Player2}) ->
              {Score1, Score2} = run_games(N, Player1, Player2),
              [{Player1, Score1}, {Player2, Score2}]
      end,
      AllPairs),
    lists:map(fun stop_player/1, Players),
    AllScores1 = lists:flatten(AllScores),
    lists:foreach(fun({Player, Score}) ->
                          io:format("~s: ~w points~n", [Player#player.name,
                                                        Score]) 
                  end,
                  AllScores1),
    ScoresByPlayer = 
        lists:foldl(
          fun({Player, Score}, Dict) ->
                  dict:update(Player,
                              fun(TotalScore) -> TotalScore + Score end,
                              Score,
                              Dict)
          end,
          dict:new(),
          AllScores1),
    ScoresList = dict:to_list(ScoresByPlayer),
    Sorted = lists:reverse(lists:keysort(2, ScoresList)),
    [{Winner, Score} | _Rest] = Sorted,
    io:format("Winner: ~s (~p points)~n~n", [Winner#player.name, Score]),
    io:format("~p ~n", [Sorted]),
    ok.

all_pairs(Lst) ->
    all_pairs(Lst, []).

all_pairs([], Lst) ->
    Lst;
all_pairs([H|T], Lst) ->
    SomePairs = lists:map(fun(Elt) -> {H, Elt} end, T),
    all_pairs(T, lists:append(Lst, SomePairs)).
    
%% Run N games between two players, returning a tuple of total points for each:
%%
%% { PointsFor1, PointsFor2 }
run_games(N, Player1, Player2) ->
    run_games(N, Player1, Player2, {0,0}).

run_games(0, _Player1, _Player2, Scores) ->
    Scores;
run_games(N, Player1, Player2, {Score1, Score2}) ->
    {{_Player1Choice, Player1Points}, 
     {_Player2Choice, Player2Points}} = play_one_game(Player1, Player2),
    run_games(N-1, Player1, Player2, {Score1 + Player1Points,
                                      Score2 + Player2Points}).


%% Run a single game between two player processes, return a tuple that looks
%% like:
%%
%% {{ Player1Choice, Player1Points }, { Player2Choice, Player2Points }}
%%
%% Where points is the number of points they score in the game.

play_one_game(Player1, Player2) ->
    Player1Choice = make_choice(Player1, Player2),
    Player2Choice = make_choice(Player2, Player1),
    send_result(Player1, {Player2, Player1Choice, Player2Choice}),
    send_result(Player2, {Player1, Player2Choice, Player1Choice}),
    {Player1Points, Player2Points} = get_points(Player1Choice, Player2Choice),
    {{Player1Choice, Player1Points}, 
     {Player2Choice, Player2Points}}.
   
%% Return the scores for each player, given their choices
get_points(Player1Choice, Player2Choice) ->
    case { Player1Choice, Player2Choice } of
        { cooperate, cooperate } -> {  8, 8 };
        { defect, cooperate    } -> { 10, 0 };
        { cooperate, defect    } -> {  0, 10 };
        { defect, defect       } -> {  4, 4 }
    end.
        
        

%% RPC calls into the player process
start_player(Module) ->
    spawn(fun() -> server_loop(Module, Module:init()) end).
    
make_choice(Player, Opponent) ->
    P = Player#player.process,
    P ! { self(), play, Opponent },
    receive 
        { P, choice, Choice } ->
            Choice
    end
        .

send_result(Player, {Opponent, OwnChoice, OpponentChoice}) ->
    Player#player.process ! { result, {Opponent, OwnChoice, OpponentChoice} }.

stop_player(Player) ->
    Player#player.process ! stop.

%% Implementation of server loop on top of functional strategy modules
server_loop(Module, State) ->
    receive 
        { DriverPid, play, Opponent } ->
            { Choice, NewState } = Module:play(Opponent, State),
            DriverPid ! { self(), choice, Choice },
            server_loop(Module, NewState) ;

        { result, {Opponent, OwnChoice, OpponentChoice} } ->
            NewState = Module:result({Opponent, OwnChoice, OpponentChoice},
                                     State),
            server_loop(Module, NewState) ;
        
        stop ->
            Module:stop(State)
    end
        .
            
            
