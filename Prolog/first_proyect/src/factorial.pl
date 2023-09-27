factorial(0, 1).
factorial(X, F) :- nonvar(X),
                   var(F),
                   X > 0,
                   Z is X-1, 
                   factorial(Z, Y),
                   F is (Y * X).
