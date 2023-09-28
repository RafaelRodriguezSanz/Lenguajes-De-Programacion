zip([], [], []).
zip([], [_|_], []).
zip([_|_], [], []).
zip([A|ColaA], [B|ColaB], [pair(A,B)|ColaResult]) :- zip(ColaA, ColaB, ColaResult).
