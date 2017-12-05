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
contenido(T,F,C,X):-rango(T,F,C),obtener(T,F,C,X).

%disponible(+Tablero, ?Fila, ?Columna)
disponible(T,F,C):-libre(T,F,C),forall(adyacenteEnRango(T,F,C,F1,C1),libre(T,F1,C1)).

%puedoColocar(+CantPiezas, ?Direccion, +Tablero, ?Fila, ?Columna)
puedoColocar(Cant,D,T,F,C):-rango(T,F,C),mover(Cant,D,T,F,C).

%ubicarBarcos(+Barcos, +?Tablero)
ubicarBarcos([],_).
ubicarBarcos([1|Xs],T):-puedoColocar(1,vertical,T,F,C),contenido(T,F,C,o),ubicarBarcos(Xs,T).
ubicarBarcos([X|Xs],T):-X\=1,puedoColocar(X,D,T,F,C),ubicarBarco(X,D,T,F,C),ubicarBarcos(Xs,T).

%completarConAgua(+?Tablero)
completarConAgua(T) :- maplist(completarConAguaFila,T).

%golpear(+Tablero, +NumFila, +NumColumna, -NuevoTab)
golpear(T,F,C,X):-rango(T,F,C),matriz(T,F1,C1),matriz(X,F1,C1),contenido(X,F,C,~),maplist(copiarFila,T,X).

% Completar instanciación soportada & justificar.
%atacar(+Tablero, +Fila, +Columna, -Resultado, -NuevoTab)
atacar(T,F,C,agua,T):-contenido(T,F,C,~).
atacar(T,F,C,hundido,N):-contenido(T,F,C,o),forall(adyacenteEnRango(T,F,C,F1,C1),contenido(T,F1,C1,~)),golpear(T,F,C,N).
atacar(T,F,C,tocado,N):-contenido(T,F,C,o),adyacenteEnRango(T,F,C,F1,C1),contenido(T,F1,C1,o),!,golpear(T,F,C,N).

%las filas ,las columnas y el resultado pueden estar instanciados o no. Pero si se encuentra una respuesta en que res=="tocado" el programa va a terminar debido al cut.
%En caso de que nada venga instanciado va a generar todas las matrices de 1 X N , con N en [1...] y siempre res va a ser "agua".
%En el caso de que venga instanciado el NuevoTab pero no Tablero, cuando res sea "agua" no va a tener problema ya que NuevoTab es igual a Tablero, pero si
%res no es "agua" se va a intentar crear todos los tableros posibles pero se va a trabar por que se van a crear todos los tableros que son de la forma 1xN,con N en [1...], por lo cual nunca va a terminar
%------------------Predicados auxiliares:------------------%

%libre(+?Tablero,?Fila,?Columna)
libre(T,F,C):-contenido(T,F,C,Y),var(Y).

%obtener(+Tablero, +Fila, +Columna,-Contenido)
obtener(T,F,C,X):-nth1(F,T,L),nth1(C,L,X).

%mover(+Cantidad,?Direccion,+?Tablero, ?Fila, ?Columna)
mover(Cant,vertical,T,F,C):-F1 is F+Cant-1,forall(between(F,F1,X),disponible(T,X,C)).
mover(Cant,horizontal,T,F,C):-C1 is C+Cant-1,forall(between(C,C1,X),disponible(T,F,X)).

%ubicarBarco(+Cantidad,+Direccion,+?Tablero,+Fila,+Columna), como precondicion se tiene que poder ubicar en un lugar valido
ubicarBarco(1,_,T,F,C):-contenido(T,F,C,o).
ubicarBarco(Cant,vertical,T,F,C):-contenido(T,F,C,o),Cant1 is Cant-1,F1 is F+1,ubicarBarco(Cant1,vertical,T,F1,C).
ubicarBarco(Cant,horizontal,T,F,C):-contenido(T,F,C,o),Cant1 is Cant-1,C1 is C+1,ubicarBarco(Cant1,horizontal,T,F,C1).

%completarConAguaFila(+?Lista)
completarConAguaFila(L):-maplist(ubicarAgua,L).

%ubicarAgua(?Var)
ubicarAgua(V):-nonvar(V).
ubicarAgua(V):-var(V), V = '~'.

%copiarFila(+Lista1,+?Lista2)
copiarFila(T,X):-maplist(copiar,T,X).

%copiar(+Var1,+?Var2)
copiar(_,X):-nonvar(X).
copiar(T,X):-var(X),T = X.

%rango(+?T,?F,?C)
rango(T,F,C):- matriz(T,X,Y),between(1,X,F),between(1,Y,C).
%------------------Tests:------------------%

test(1) :- matriz(M,2,3), adyacenteEnRango(M,2,2,2,3).
test(2) :- matriz(M,2,3), setof((F,C), adyacenteEnRango(M,1,1,F,C), [ (1, 2), (2, 1), (2, 2)]).
test(3) :-contenido([[o, ~], [o, ~], [o, ~]],1,1,o).
test(4) :- matriz(M,3,3), setof((F,C), disponible(M,F,C), [ (1, 1), (1, 2), (1, 3), (2, 1), (2, 2), (2, 3), (3, 1), (3, 2), (3, 3)]).
test(5) :- matriz(M,3,3), contenido(M,1,1,o),contenido(M,3,1,o),setof((F,C),puedoColocar(2,_,M,F,C), [(1, 3), (2, 3)]).
test(6) :- matriz(M,3,3),ubicarBarco(3,vertical,M,1,1),completarConAgua(M),golpear(M,1,1,[[~, ~, ~], [o, ~, ~], [o, ~, ~]]).
test(7) :- matriz(M,3,3),contenido(M,3,3,o),ubicarBarco(3,vertical,M,1,1),completarConAgua(M),setof((Res,T),atacar(M,1,1,Res,T),[(tocado , [[~, ~, ~], [o, ~, ~], [o, ~, o]])]).
test(8) :- matriz(M,3,3),contenido(M,3,3,o),ubicarBarco(3,vertical,M,1,1),completarConAgua(M),setof((Res,T),atacar(M,2,2,Res,T),[(agua , [[o, ~, ~], [o, ~, ~], [o, ~, o]])]).
test(9) :- matriz(M,3,3),contenido(M,3,3,o),ubicarBarco(3,vertical,M,1,1),completarConAgua(M),setof((Res,T),atacar(M,3,3,Res,T),[(hundido , [[o, ~, ~], [o, ~, ~], [o, ~,  ~]])]).
test(10) :- matriz(M,3,3),contenido(M,3,3,o),ubicarBarco(3,vertical,M,1,1),completarConAgua(M),setof((Res,T),(atacar(M,1,1,_,M1),atacar(M1,2,1,_,M2),atacar(M2,3,1,Res,T)),[(hundido , [[~, ~, ~], [~, ~, ~], [~, ~, o]])]).
tests :- forall(between(1,10,N), test(N)). % Cambiar el 2 por la cantidad de tests que tengan.
