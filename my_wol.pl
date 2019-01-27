:- [war_of_life].

test_strategy(N, S1, S2) :-
	count_score(N, S1, S2, _, _, _, _, _, _, _). 

count_score(N, S1, S2, NumDraws, NumWinsB, NumWinsR, Longest, Shortest, AvgLength, AvgTime) :-
	N > 0,
	play(quiet, S1, S2, NumMoves, WinningPlayer),
	(NumMoves > Longest -> LongestNew is NumMoves ; LongestNew is Longest),
	(NumMoves < Shortest -> ShortestNew is NumMoves ; ShortestNew is Shortest),

	count_score(N-1, S1, S2, NumDraws, NumWinsB, NumWinsR, LongestNew, ShortestNew, AvgLength, AvgTime).

count_score(0, _, _, NumDraws, NumWinsB, NumWinsR, Longest, Shortest, AvgLength, AvgTime) :-
	write(NumDraws),
	write(NumWinsB),
	write(NumWinsR),
	write(Longest),
	write(Shortest),
	write(AvgLength),
	write(AvgTime).



%%%%%%USE count_score to loop and accumluate stuff???


%write() --print to console
%use accumulator??
%while loop
%aggregate_all(count, is_man(X), Count).
%IDK WHAT I AM DOING
