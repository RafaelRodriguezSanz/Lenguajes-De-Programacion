:- consult('../src/rule3.pl').

test_rule3 :-
    writeln('Prueba Rule3>'),
    prueba_rule3_1,
    prueba_rule3_2,
    prueba_rule3_3,
    prueba_rule3_4,
    writeln('------------------').

prueba_rule3_1 :-
    rule3(2, 3, 6, D),
    D = 9,
    writeln('Prueba 1 pasada').

prueba_rule3_2 :-
    rule3(A, 3, 6, 9),
    A = 2,
    writeln('Prueba 2 pasada').

prueba_rule3_3 :-
    rule3(2, B, 6, 9),
    B = 3,
    writeln('Prueba 3 pasada').

prueba_rule3_4 :-
    rule3(2, 3, C, 9),
    C = 6,
    writeln('Prueba 4 pasada').