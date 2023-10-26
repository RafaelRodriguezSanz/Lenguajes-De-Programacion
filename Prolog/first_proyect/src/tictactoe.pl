tictactoe(A, B, C, D, E, F, G, H, I).

start(T) :- T = (empty, empty, empty, empty, empty, empty, empty, empty, empty).

winner(tictactoe(X, X, X, _, _, _, _, _, _), X):- X \= empty.
winner(tictactoe(_, _, _, X, X, X, _, _, _), X):- X \= empty.
winner(tictactoe(_, _, _, _, _, _, X, X, X), X):- X \= empty.
winner(tictactoe(X, _, _, X, _, _, X, _, _), X):- X \= empty.
winner(tictactoe(_, X, _, _, X, _, _, X, _), X):- X \= empty.
winner(tictactoe(X, _, _, _, X, _, _, _, X), X):- X \= empty.
winner(tictactoe(X, _, _, _, _, X, _, _, X), X):- X \= empty.
winner(tictactoe(_, _, X, _, X, _, X, _, _), X):- X \= empty.