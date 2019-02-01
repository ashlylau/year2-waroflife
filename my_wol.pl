%:- [war_of_life].

%%%%%%%%%%%%%%% test_strategy/3 %%%%%%%%%%%%%%%%%
test_strategy(N, S1, S2) :-
	count_score(N, S1, S2, NumDraws, NumWinsB, NumWinsR, Longest, Shortest, TotalLength, TotalTime), 
	AvgLength is TotalLength/N,
	AvgTime is TotalTime/N,
	format('NumDraws: ~w ~n', [NumDraws]),
	format('NumWinsB: ~w ~n', [NumWinsB]),
	format('NumWinsR: ~w ~n', [NumWinsR]),
	format('Longest: ~w ~n', [Longest]),
	format('Shortest: ~w ~n', [Shortest]),
	format('AvgLength: ~w ~n', [AvgLength]),
	format('AvgTime: ~w ~n', [AvgTime]).

%%%%%%%%%%%%%%% count_score/10 %%%%%%%%%%%%%%%%%%
count_score(N, S1, S2, NumDraws, NumWinsB, NumWinsR, Longest, Shortest, TotalLength, TotalTime) :-
	N > 0,
	NPrev is N-1,

	statistics(runtime, [Start|_]),
	play(quiet, S1, S2, NumMoves, WinningPlayer),
	statistics(runtime, [Stop|_]),
	Runtime is Stop - Start,

	count_score(NPrev, S1, S2, NumDrawsPrev, NumWinsBPrev, NumWinsRPrev, LongestPrev, ShortestPrev, TotalLengthPrev, TotalTimePrev),

  (NumMoves > LongestPrev -> Longest is NumMoves ; Longest is LongestPrev),
	(NumMoves < ShortestPrev -> Shortest is NumMoves ; Shortest is ShortestPrev),
	(WinningPlayer == draw -> NumDraws is NumDrawsPrev+1 ; NumDraws is NumDrawsPrev),
	(WinningPlayer == b -> NumWinsB is NumWinsBPrev+1 ; NumWinsB is NumWinsBPrev),
	(WinningPlayer == r -> NumWinsR is NumWinsRPrev+1 ; NumWinsR is NumWinsRPrev),
	TotalLength is TotalLengthPrev + NumMoves,
	TotalTime is TotalTimePrev + Runtime.

count_score(0, _, _, 0, 0, 0, 0, 250, 0, 0). %arbitrarily large max int for initialisation 




%%%%%%%%%%%%%%%%% STRATEGIES %%%%%%%%%%%%%%%%%%%%%

% bloodlust(+PlayerColour, +CurrentBoardState, -NewBoardState, -Move)
% chooses next move which (after Conway's crank) produces the board state with the fewest number of opponent's pieces
bloodlust('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveBlues),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveBlues),
												\+member([MA,MB], AliveReds)),
			    PossMoves),
	min_pieces('b', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveBlues, NewAliveBlues).

bloodlust('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveReds),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveReds),
												\+member([MA,MB], AliveBlues)),
			    PossMoves),
	min_pieces('r', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveReds, NewAliveReds).


% chooses Move out of list of Moves that generates board state with
% fewest number of opponents pieces after turning Conway's crank
% min_pieces(Colour, CurrentBoard, Moves, Move, MinNumOppPiece)
min_pieces(Colour, [AliveBlues, AliveReds], [M|Moves], Move, Min) :-
	(Colour == 'b' ->	alter_board(M, AliveBlues, NewAliveBlues),
										next_generation([NewAliveBlues, AliveReds], [_, CrankedAliveReds]),
										length(CrankedAliveReds, NumOpp),
										min_pieces('b', [AliveBlues, AliveReds], Moves, PrevMove, PrevMin)
										;
										alter_board(M, AliveReds, NewAliveReds),
										next_generation([AliveBlues, NewAliveReds], [CrankedAliveBlues, _]),
										length(CrankedAliveBlues, NumOpp),
										min_pieces('r', [AliveBlues, AliveReds], Moves, PrevMove, PrevMin)),
	(NumOpp < PrevMin -> Move = M, Min is NumOpp; Move = PrevMove, Min is PrevMin).  

min_pieces(_, _, [], _, 64).



% self preservation
% chooses next move which (after Conway's crank) produces the board state with the largest number of that player's pieces on the board
%self_preservation().
self_preservation('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveBlues),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveBlues),
												\+member([MA,MB], AliveReds)),
			    PossMoves),
	max_pieces('b', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveBlues, NewAliveBlues).

self_preservation('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveReds),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveReds),
												\+member([MA,MB], AliveBlues)),
			    PossMoves),
	max_pieces('r', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveReds, NewAliveReds).


% chooses Move out of list of Moves that generates board state with 
% most number of player's pieces after turning Conway's Crank
% max_pieces(Colour, CurrentBoard, Moves, Move, MaxNumPlayerPiece).
max_pieces(Colour, [AliveBlues, AliveReds], [M|Moves], Move, Max) :-
	(Colour == 'b' ->	alter_board(M, AliveBlues, NewAliveBlues),
										next_generation([NewAliveBlues, AliveReds], [CrankedAliveBlues, _]),
										length(CrankedAliveBlues, NumPlayer),
										max_pieces('b', [AliveBlues, AliveReds], Moves, PrevMove, PrevMax)
										;
										alter_board(M, AliveReds, NewAliveReds),
										next_generation([AliveBlues, NewAliveReds], [_, CrankedAliveReds]),
										length(CrankedAliveReds, NumPlayer),
										max_pieces('r', [AliveBlues, AliveReds], Moves, PrevMove, PrevMax)),
	(NumPlayer > PrevMax -> Move = M, Max is NumPlayer; Move = PrevMove, Max is PrevMax).
	
max_pieces(_, _, [], _, 0).




% land grab
% choose next move which (after Conway's crank) produces the board state which maximises this function: NumPlayersPieces - NumOpponentPieces
%land_grab(Colour, CurrentBoard, NextBoard, Move)
land_grab('b', [AliveBlues, AliveReds], [NewAliveBlues, AliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveBlues),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveBlues),
												\+member([MA,MB], AliveReds)),
			    PossMoves),
	max_function('b', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveBlues, NewAliveBlues).


land_grab('r', [AliveBlues, AliveReds], [AliveBlues, NewAliveReds], Move) :-
	findall([A,B,MA,MB], (member([A,B], AliveReds),
												neighbour_position(A,B,[MA,MB]),
												\+member([MA,MB], AliveReds),
												\+member([MA,MB], AliveBlues)),
			    PossMoves),
	max_function('r', [AliveBlues, AliveReds], PossMoves, Move, _),
	alter_board(Move, AliveReds, NewAliveReds).

% max_function(Colour, CurrentBoard, Moves, Move, MaxFunctionValue)
max_function('b', [AliveBlues, AliveReds], [M|Moves], Move, Max) :-
	alter_board(M, AliveBlues, NewAliveBlues),
	next_generation([NewAliveBlues, AliveReds], [CrankedAliveBlues, CrankedAliveReds]),
	length(CrankedAliveBlues, NumPlayer),
	length(CrankedAliveReds, NumOpp),
	max_pieces('b', [AliveBlues, AliveReds], Moves, PrevMove, PrevMax),

	Diff is NumPlayer - NumOpp,
	(Diff > PrevMax -> Move = M, Max is Diff; Move = PrevMove, Max is PrevMax).

max_function('r', [AliveBlues, AliveReds], [M|Moves], Move, Max) :-
	alter_board(M, AliveReds, NewAliveReds),
	next_generation([AliveBlues, NewAliveReds], [CrankedAliveBlues, CrankedAliveReds]),
	length(CrankedAliveReds, NumPlayer),
	length(CrankedAliveBlues, NumOpp),
	max_pieces('r', [AliveBlues, AliveReds], Moves, PrevMove, PrevMax),

	Diff is NumPlayer - NumOpp,
	(Diff > PrevMax -> Move = M, Max is Diff; Move = PrevMove, Max is PrevMax).

max_function(_, _, [], _, 0).




% minimax
% strategy looks two-ply ahead using the heuristic measure described in the landgrab strategy. 
% it should follow the minimax principle and take into account the opponent's move after the one chosen for the current player.
%minimax().




