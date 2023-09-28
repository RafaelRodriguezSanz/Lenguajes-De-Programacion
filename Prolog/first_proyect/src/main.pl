init :-
    
    % Ejemplo de regla de tres
    rule3(1, 3, 6, X),
    writeln('Ejemplo de regla de tres:'),
    writeln('A = 1'),
    writeln('B = 3'),
    writeln('C = 6'),
    write('X = '), writeln(X),
    writeln('---'),

    % Ejemplo de lenguaje
    language(ut2, L),
    writeln('Ejemplo de lenguaje:'),
    writeln('UT = ut2'),
    write('L = '), writeln(L),
    writeln('---'),

    % Ejemplo de factorial
    factorial(5, F),
    writeln('Ejemplo de factorial:'),
    writeln('X = 5'),
    write('F = '), writeln(F),
    writeln('---'),

    % Ejemplo de zip
    zip([1, 2, 3], [a, b, c], Resultado),
    writeln('Ejemplo de zip:'),
    writeln('Lista1 = [1, 2, 3]'),
    writeln('Lista2 = [a, b, c]'),
    write('Resultado = '), writeln(Resultado),
    writeln('---').

% Import
:- [factorial].
:- [rule3].
:- [ut3].
:- [zip].