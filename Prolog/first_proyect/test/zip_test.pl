:- consult('../src/zip.pl').

test_zip :-
    writeln('Prueba Zip>'),
    prueba1,
    prueba2,
    prueba3,
    prueba4,
    prueba5,
    prueba6,
    writeln('------------------').

prueba1 :-
    zip([], [], Resultado),
    Resultado = [],
    writeln('Prueba 1 prueba_zip_').

prueba2 :-
    zip([], [1], Resultado),
    Resultado = [],
    writeln('Prueba 2 prueba_zip_').

prueba3 :-
    zip([1], [], Resultado),
    Resultado = [],
    writeln('Prueba 3 prueba_zip_').

prueba4 :-
    zip([1], [2], Resultado),
    Resultado = [pair(1,2)],
    writeln('Prueba 4 prueba_zip_').

prueba5 :-
    zip([1], [2,3], Resultado),
    Resultado = [pair(1,2)],
    writeln('Prueba 5 prueba_zip_').

prueba6 :-
    zip([1,2], [3,4], Resultado),
    Resultado = [pair(1,3), pair(2,4)],
    writeln('Prueba 6 prueba_zip_').