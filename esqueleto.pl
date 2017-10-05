%------------------Predicados predefinidos:------------------%

%fliplength(?Longitud, ?Lista)
fliplength(N, L) :- length(L, N).

%matriz(?Matriz, ?Filas, ?Columnas)
matriz(M, F, C) :- length(M, F), maplist(fliplength(C), M).

%dif1(+N1, ?N2)
dif1(N1, N2) :- N2 is N1 + 1.
dif1(N1, N2) :- N2 is N1 - 1.

%adyacente(+F1, +C1, ?F2, ?C2)
adyacente(F1,C1,F1,C2) :- dif1(C1,C2).
adyacente(F1,C1,F2,C1) :- dif1(F1,F2).
adyacente(F1,C1,F2,C2) :- dif1(C1,C2), dif1(F1,F2).

%enRango(+Matriz, +Fila, +Columna)
enRango([Fila|Filas], F, C) :- F > 0, C > 0, length([Fila|Filas], FMax), F =< FMax, length(Fila, CMax), C =< CMax.

%adyacenteEnRango(+Tablero, +F1, +C1, ?F2, ?C2)
adyacenteEnRango(T,F1,C1,F2,C2) :- adyacente(F1,C1,F2,C2), enRango(T,F2,C2).

%------------------Predicados a definir:------------------%


%contenido(+?Tablero, ?Fila, ?Columna, ?Contenido)
contenido(T,F,C,X):-valido(T,F,C),obtener(T,F,C,X).
contenido(T,F,C,X):-not(ground(F)),ground(C),matriz(T,F1,_),between(1,F1,F),obtener(T,F,C,X).
contenido(T,F,C,X):-not(ground(C)),ground(F),matriz(T,_,C1),between(1,C1,C),obtener(T,F,C,X).
contenido(T,F,C,X):-not(ground(F)),not(ground(C)),matriz(T,F1,C1),between(1,F1,F),between(1,C1,C),obtener(T,F,C,X).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T,F,C):-valido(T,F,C),libre(T,F,C),forall(adyacenteEnRango(T,F,C,F1,C1),libre(T,F1,C1)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(Cant,D,T,F,C):-valido(T,F,C),mover(Cant,D,T,F,C).
puedoColocar(Cant,D,T,F,C):-not(ground(F)),ground(C),matriz(T,F1,_),between(1,F1,F),mover(Cant,D,T,F,C).
puedoColocar(Cant,D,T,F,C):-not(ground(C)),ground(F),matriz(T,_,C1),between(1,C1,C), mover(Cant,D,T,F,C).
puedoColocar(Cant,D,T,F,C):-not(ground(F)),not(ground(C)),matriz(T,F1,C1),between(1,F1,F),between(1,C1,C),mover(Cant,D,T,F,C).

%ubicarBarcos(+Barcos, +?Tablero)

%completarConAgua(+?Tablero)
completarConAgua(T):-forall(libre(T,X,Y),contenido(T,X,Y,"~")).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)

% Completar instanciación soportada y justificar.
%atacar(Tablero, Fila, Columna, Resultado, NuevoTab)

%------------------Predicados auxiliares:------------------%

%valido(+?Tablero, ?Fila, ?Columna)
valido(T,F,C):-ground(F),ground(C),matriz(T,F1,C1),F=<F1,C=<C1.

%libre(+?Tablero,?Fila,?Columna)
libre(T,F,C):-contenido(T,F,C,Y),not(ground(Y)).

%obtener(+Tablero, +Fila, +Columna,-Contenido)
obtener(T,F,C,X):-nth1(F,T,L),nth1(C,L,X).

%mover(+Cantiad,+Direccion,+Tablero, +F1, +C1)
mover(Cant,vertical,T,F,C):-F1 is F+Cant-1,forall(between(F,F1,X),disponible(T,X,C)).
mover(Cant,horizontal,T,F,C):-C1 is C+Cant-1,forall(between(C,C1,X),disponible(T,F,X)).

%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
tests :- forall(between(1,2,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
