:- [war_of_life].

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

count_score(0, _, _, 0, 0, 0, 0, 1000, 0, 0). %arbitrarily large max int 


%%%%%%USE count_score to loop and accumluate stuff???


%write() --print to console
%use accumulator??
%while loop
%aggregate_all(count, is_man(X), Count).
%IDK WHAT I AM DOING
%%%





%%%%%%%% STRATEGIES %%%%%%%%%

% bloodlust(PlayerColour, CurrentBoardState, Move)
% chooses next move which (after Conway's crank) produces the board state with the fewest number of opponent's pieces
%bloodlust().


% self preservation
% chooses next move which (after Conway's crank) produces the board stte with teh largest number of that player's pieces on the board
%self_preservation().


% land grab
% choose next move which (after Conway's crank) produces the board state which maximises this function: NumPlayersPieces - NumOpponentPieces
%land_grab().


% minimax
% strategy looks two-ply ahead using the heuristic measure described in the landgrab strategy. 
% it should follo wthe minimax principle and take into account the opponent's move after the one chosen for the current player.
%minimax().




