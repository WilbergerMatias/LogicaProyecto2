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
    color(Grid,[PosX, PosY],Elem),
    dif(Color,Elem),
    calcularAdyacentes(Grid, [PosX, PosY], ListadeAdyacentes),
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

cambiarColorAdyacentes(Grid, [[PosX , PosY] | Resto], Color, PLista) :-
    encontrarLista(Grid,PosX,Lista),
    cambiarElemento(PosY, Lista, Color, ListaS),
    cambiarElemento(PosX, Grid, ListaS, NuevaLista),
    cambiarColorAdyacentes(NuevaLista, Resto, Color, PLista).

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





%!
% Predicado principal de la ayuda, toma +Grilla, +Colores, [+PosX,
% +PosY], +ColPrincipal, +Profundidad, -MejorSolcion, -Capturadas.
% +Grilla: es la grilla actual del juego, necesaria para poder realizar
% flicks
% +ColPrincipal: color de la celda principal.
% [+PosX, +PosY]: lista con la posicion de la celda en la grilla.
% +Colores: lista de los colores del juego, utilizado al realizar
% flicks.
% +Profundidad: cantidad de movidas a realizar.
% -MejorSolucion: lista con [Solucion, capturadas], donde solucion es la
% secuencia de movidas que se encontro, y capturadas la cantidad de
% celdas que logra capturar.
ayuda(Grilla, Colores, [PosX,PosY], ColPrincipal, Profundidad, MejorSolucion):-
    length(Grilla, L),
    Total is L*L,
    assert(celdas(Total)),
    findall(
        [Solucion, CantCapturadas],
        greedSearch(Grilla, Colores, [PosX,PosY], ColPrincipal, Profundidad, Solucion, CantCapturadas),
        Soluciones),
    findall(
        [SolucionTermina, Total],
        member([SolucionTermina, Total], Soluciones),
        Terminan),
    filtrarSolucionMayorCapt(Soluciones, MasCapt),
    filtrarSolucionTerminanMasCorta(Terminan, MejorTermina),
    encontrarSolucion(MasCapt,MejorTermina, MejorSolucion),
    retract(celdas(_)).

encontrarSolucion(Sol1,[],Sol1).

encontrarSolucion(_, SolTermina, SolTermina).

%!
% Predicado encargado de encontrar entre las soluciones que se tienen,
% cual es la que mas celdas captura. (util en caso de que ninguna
% jugada encontrada, termine el juego).
filtrarSolucionMayorCapt([], [[], 0]).
filtrarSolucionMayorCapt([[Jugada, Capturadas]|RestoSoluciones],[MejJugada,MasCapt]):-
    encontrarMejor(RestoSoluciones, Jugada, Capturadas, [MejJugada, MasCapt]).

%!
% Este predicado se encarga de comparar las jugadas y mantere cual es la
% mejor hasta el momento.
encontrarMejor([], Jugada, Capturadas, [Jugada, Capturadas]).

encontrarMejor([[Jugada, Capturadas]|RestoSoluciones], _MejJugActual, MasCaptAct,  [MejJugada,MasCapt]):-
    Capturadas>=MasCaptAct,
    encontrarMejor(RestoSoluciones, Jugada, Capturadas, [MejJugada, MasCapt]).

encontrarMejor([[_Jugada, _Capturadas]|RestoSoluciones], MejJugActual, MasCaptAct,  [MejJugada,MasCapt]):-
     encontrarMejor(RestoSoluciones, MejJugActual, MasCaptAct, [MejJugada, MasCapt]).

%!
% Este predicado se encarga de tomar, de las soluciones que terminan el
% juego, la que tenga la menor secuencia de jugadas.
filtrarSolucionTerminanMasCorta([[Jugada, Capturadas]|RestoSoluciones], [MejJugada,MasCapt]):-
    encontrarMenor(RestoSoluciones, Jugada, Capturadas, [MejJugada, MasCapt]).

encontrarMenor([], Jugada, Total, [Jugada,Total]).

encontrarMenor([[Jugada, Total]|RestoSoluciones], MejJugAct, Total, [MejJugada,Total]):-
    length(Jugada, Largo),
    length(MejJugAct, mejorLargo),
    Largo<mejorLargo,
    encontrarMenor(RestoSoluciones, Jugada, Total, [MejJugada, Total]).


encontarMenor([[_Jugada, Total]|RestoSoluciones], MejJugAct, Total, [MejJugada,Total]):-
    encontrarMenor(RestoSoluciones, MejJugAct, Total, [MejJugada, Total]).

%!
% Este predicado se encarga de realizar una busqueda exhaustiva para
% lograr encontrar aquellas secuencias de movidas que se pueden realizar
% en la profundidad, ignorando aquellas jugadas triviales.
greedSearch(_Grilla, _Colores, _Start, _ColPrincipal, 0, [], 0):-!.

greedSearch(Grilla, Colores, [PosX,PosY], Color, 1, [NCol], TotalCapturadas):-
    NCol\=Color,
    member(NCol, Colores),
    flick(Grilla,NCol, PosX, PosY, FGrid),
    calcularAdyacentes(FGrid,[PosX, PosY], Aux),
    length(Aux, TotalCapturadas).

greedSearch(Grilla, Colores, [PosX,PosY], Color, Profundidad, [NCol|Sol], TotalCapturadas):-
    ProfMenor is Profundidad - 1,
    ProfMenor > 0,
    NCol\=Color,
    member(NCol, Colores),
    flick(Grilla, NCol, PosX, PosY,FGrid),
    (calcularAdyacentes(FGrid,[PosX, PosY], Aux), length(Aux, CantCapturadas)),
    controlFinJuego(FGrid, Colores, [PosX,PosY], Color, NCol, CantCapturadas, ProfMenor, Sol, TotalCapturadas).


%!
% Este predicado se encarga de verificar que una jugada, termine el
% juego. En caso de que eso suceda, no es necesario seguir buscando para
% dicha jugada, aun si no se llego a la profundidad dada.
controlFinJuego(_, _, _, _,_, Total, _, _, Total):-
    celdas(Total),!.

controlFinJuego(Gridmid, Colores, [PosX, PosY], _Color, NCol, CantCapt, ProfMenor, Sol, TotalCapturadas):-
    TotalCapturadas is TotalCapturadasAux+CantCapt,
    greedSearch(Gridmid, Colores, [PosX, PosY], NCol, ProfMenor, Sol, TotalCapturadasAux).

