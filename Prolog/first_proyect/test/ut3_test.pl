:- consult('../src/ut3.pl').

test_ut3 :-
    writeln('Prueba UT3>'),
    prueba_ut3_1,
    prueba_ut3_2,
    prueba_ut3_3,
    prueba_ut3_4,
    prueba_ut3_5,
    writeln('------------------').

prueba_ut3_1 :-
    language(ut1, L),
    L = rust,
    writeln('Prueba 1 pasada').

prueba_ut3_2 :-
    language(ut2, L),
    L = haskell,
    writeln('Prueba 2 pasada').

prueba_ut3_3 :-
    language(ut3, L),
    L = prolog,
    writeln('Prueba 3 pasada').

prueba_ut3_4 :-
    language(ut4, L),
    L = ruby,
    writeln('Prueba 4 pasada').

prueba_ut3_5 :-
    language(ut5, L),
    L = javascript,
    writeln('Prueba 5 pasada').