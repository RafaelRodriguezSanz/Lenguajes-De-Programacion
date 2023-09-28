rule3(A,B,C,X) :- var(X), X is B * C / A.
rule3(A,B,C,X) :- var(A), A is B * C / X.
rule3(A,B,C,X) :- var(B), B is A * X / C.
rule3(A,B,C,X) :- var(C), C is A * X / B.