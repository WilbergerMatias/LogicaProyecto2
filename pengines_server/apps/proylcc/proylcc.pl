:- module(proylcc,
	[
		flick/5
	]).

%!
%
% Flick(+Grid, +Color, +PosX, +PosY, -FGrid) <- esto es si se guarda el estado en React
%
% Caso contrario, se debe sacar, y realizar un predicado extra (assert) para guardar
% posicion inicial, y luego consultarla.
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda superior izquierda de la grilla.

flick(Grid, Color, PosX, PosY, FGrid):-
    color(Grid,[PosX,PosY],Elem),
    dif(Color,Elem),
    calcularAdyacentes(Grid, [PosY, PosX], ListadeAdyacentes),
    cambiarColorAdyacentes(Grid, ListadeAdyacentes, Color, FGrid).

%!
% Caso base utilizado en el predicado de cambio de color

cambiarColorAdyacentes(Grid, [], _Color, Grid).

%!
% cambiarColorAdyacentes(+Grid, [[+PosX, +PosY]|+Ys], +Color, -PLista)
%
% Caso recursivo del predicado de cambio de color,
% Este predicado se encarga de cambiar el elemento en Grid,
% en posicion X e Y por el elemento Color

cambiarColorAdyacentes(Grid, [[PosY , PosX] | Ys], Color, PLista) :-
    encontrarLista(Grid,PosY,Lista),
    cambiarElemento(PosX, Lista, Color, ListaS),
    cambiarElemento(PosY, Grid, ListaS, NuevaLista),
    cambiarColorAdyacentes(NuevaLista, Ys, Color, PLista).

%!
% encontrarLista([+Lista|Resto],+Y,-Rta)
%
% Predicado para encontrar una lista por su posicion (_, Y) en una
% matriz/grid, con iniciales (0, 0) hasta (n-1, n-1).
% (Pensado para ser usado con el predicado encontrar elemento).

encontrarLis(Lista, 0, Lista).

encontrarLista([Lista|RESTO],Y,Rta):-
    (not(Y is 0), Aux is Y-1,encontrarLista(RESTO,Aux,Rta));
    (encontrarLis(Lista, 0, Rta), Y is 0).

%!
% encontrarElemento(+Lista, +X, -Elem)
%
% Predicado para econtrar el X-esimo elemento de una lista.
% Utiliza el predicado nth0/3.

encontrarElemento(Lista,X,Elem):-
	nth0(X,Lista,Elem).

%!
% cambiarElemento(+PosX, +Lista, +Color, -Res)
%
% Cambia el elemento de una lista por otro. utiliza el nth0/4,
% Utilizado para dada una lista, cambiar un color por otro y retornar la
% lista resultado. Tambien utilizado para dada una lista de listas,
% intercambiar una lista por otra y retornar la nueva lista de listas.

cambiarElemento(PosX, Lista, Color, Res) :-
  nth0(PosX, Lista, _, Aux),
  nth0(PosX, Res, Color, Aux).

%!
% compare([[+X|Xy]|+T], [+Z|+Zy])
%
% Predicado para comparar pares [X,Y], utilizado en la busqueda
% de adyacentes (para conocer si una solucion ya fue encontrada).
% Compara ambos pares, y retorna true en caso de que sean iguales.
compare([[X|Xy]|T],[Z|Zy]):-
    equal([X|Xy],[Z|Zy]);compare(T,[Z|Zy]).

%!
% equal([+X|+Xy],[+Z|+Zy])
%
equal(X,X).

%!
% calcularAdyacentes(+Grid, Start, -Adyacemtes)
% Este predicado se encarga de buscar todas las celdas adyacentes a la
% celda origen
% Grid: la grilla actual del juego.
% Start: la posicion de celda inicial/principal
% Adyacentes: la lista de celdas adyacentes a la incial.
%

calcularAdyacentes(Grid, Start, Adyacentes) :-
    findAdyacentes(Grid, [Start], [], Adyacentes).

%!
% findAdyacentes(+Grid, +Pend, +Visited, -Adyacentes)
% Pend: son las celdas que todavia no se analizaron.
% Vis:son las celdas que ya fueron consideradas.
% Grid: grilla del juego
% Adyacentes: la lista de celdas adyacentes a la incial.
%

findAdyacentes(_Grid, [], Visited, Visited).

findAdyacentes(Grid, Pend, Visited, Adyacentes):-
    Pend = [P|Ps],
    findall(A,
            (adyC(Grid, P, A), not(member(A, Pend)), not(member(A, Visited))),
            AdyCP),
    append(AdyCP, Ps, NPend),
    findAdyacentes(Grid, NPend, [P|Visited], Adyacentes).

/*
 * adyC(+P, +Grid, -A)
 */

adyC(Grid, P, A):-
    ady(Grid, P, A),
    color(Grid, P, C),
    color(Grid, A, C).

/*
 * ady(+P, +Grid, -A)
 */

ady(Grid, [X, Y], [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady(_Grid, [X, Y], [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady(Grid, [X, Y], [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady(_Grid, [X, Y], [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.


/*
 * color(P, Grid, C)
 */

color(Grid, [X,Y], C):-
    nth0(X, Grid, F),
    nth0(Y, F, C).




ayuda(Grilla, Colores, [PosX,PosY], ColPrincipal, Profundidad, MejorSolucion, Capturadas):-
    length(Grid, L),
    Total is L*L,
    assert(celdas(Total)),
    findall(
        [Solucion, CantCapturadas],
        greedSearch(Grilla, Colores, [PosX,PosY], ColPrincipal, Profundidad, Solucion, CantCapturadas),
        Res),

    %%FALTA COMPARAR SOLUCIONES ENCONTRADAS, SI HAY SOLUCION QUE TERMINA JUEGO AGARRAR LA MAS CORTA, SI NO, DEVOLVER LA QUE MAS CAPTURA. ESTO SE PUEDE HACER CON UN FINDALL PARA VER QUE SOLUCIONES COMPLETAN EL JUEGO.
    retract(celdas(_)).


greedSearch(Grilla, Colores, Start, ColPrincipal, 0, [], 0):-!.

greedSearch(Grilla, Colores, [PosX,PosY], Color, 1, [NCol], TotalCapturadas):-
    NCol\=Color,
    member(NCol, Colores),
    flick(Grilla,NCol, PosX, PosY, FGrid).

greedSearch(Grilla, Colores, [PosX,PosY], Color, Profundidad, [NCol|Sol], TotalCapturadas):-
    ProfMenor is Profundidad - 1,
    ProfMenor > 0,
    NCol\=Color,
    member(NCol, Colores),
    flick(Grilla, NCol, PosX, PosY,FGrid),
    (calcularAdyacentes(FGrid,[PosX, PosY], Aux), CantCapturadas is length(Aux)),
    controlFinJuego(FGrid, Colores, [PosX,PosY], Color, NCol, CantCapturadas, ProfMenor, Sol,TotalCapturadas).


controlFinJuego(_, _, _, _,_, Total, _, _, Total):-
    celdas(Total),!.

controlFinJuego(Gridmid, Colores, [PosX, PosY], _Color, NCol, _CantCapt, ProfMenor, Sol, TotalCapturadas):-
    greedSearch(Gridmid, Colores, [PosX, PosY], NCol, ProfMenor, Sol, TotalCapturadas).

