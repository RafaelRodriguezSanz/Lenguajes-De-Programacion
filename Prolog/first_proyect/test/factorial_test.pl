:- consult('../src/factorial.pl').

test_factorial :-
    writeln('Prueba Factorial>'),
    prueba_factorial_1,
    prueba_factorial_2,
    prueba_factorial_3,
    writeln('------------------').

prueba_factorial_1 :-
    factorial(0, Resultado),
    Resultado = 1,
    writeln('Prueba 1 pasada').

prueba_factorial_2 :-
    factorial(5, Resultado),
    Resultado = 120,
    writeln('Prueba 2 pasada').

prueba_factorial_3 :-
    factorial(4, Resultado),
    Resultado = 24,
    writeln('Prueba 3 pasada').