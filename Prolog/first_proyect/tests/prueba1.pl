:- consult('../src/main.pl').

prueba1 :-
    iniciar,
    assertion(predicado_de_modulo1),
    writeln('Prueba 1 pasada').

prueba2 :-
    iniciar,
    assertion(predicado_de_modulo2),
    writeln('Prueba 2 pasada').
