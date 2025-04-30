% Configurar la codificación de entrada y salida como UTF-8
:- set_prolog_flag(encoding, utf8).

:- dynamic opcion/2.
:- dynamic jugador/3.
:- dynamic partida_activa/1.
:- dynamic casilla/4.
:- dynamic ficha_disponible/2.
:- dynamic historial/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONFIGURACIÓN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% opcion_valida(+Opcion, -ValoresPosibles)
opcion_valida(idioma, ['es', 'eus', 'en']).
opcion_valida(modo_juego, ['jugadorVSjugador', 'jugadorVSmaquina']).
opcion_valida(reparto_fichas, ['aleatorio', 'manual']).
opcion_valida(modo_inicio, ['normal', 'alterno']).

% ver_opcion(+Opcion)
ver_opcion(O) :-
    opcion(O, V),
    format("~w: ~w~n", [O, V]).

% establecer_opcion(+Opcion, +Valor)
establecer_opcion(O, V) :-
    \+ partida_activa(_),
    opcion_valida(O, ValoresPosibles), % Consulta los valores válidos
    (   member(V, ValoresPosibles)    % Verifica si el valor es válido
    ->  (retract(opcion(O, _)) ; true),
        assert(opcion(O, V))
    ;   format("Error: ~w no es un valor válido para la opción ~w.~n", [V, O]), fail
    ).

% configurar_opciones/0: predicado auxiliar para la configuración automática de todas las opciones por consola.
configurar_opciones :-
    preguntar_opcion(idioma, "Seleccione el idioma (es/eus/en): "),
    preguntar_opcion(modo_juego, "Seleccione el modo de juego (jugadorVSjugador/jugadorVSmaquina): "),
    preguntar_opcion(reparto_fichas, "Seleccione el reparto de fichas (aleatorio/manual): "),
    preguntar_opcion(modo_inicio, "Seleccione el modo de inicio (normal/alterno): ").

% preguntar_opcion(+Opcion, +Mensaje)
preguntar_opcion(Opcion, Mensaje) :-
    format(Mensaje),
    flush_output,
    read_line_to_string(user_input, ValorString),
    atom_string(Valor, ValorString),
    (   establecer_opcion(Opcion, Valor)
    ->  true
    ;   format("Valor no válido. Inténtelo de nuevo.~n~n"),
        preguntar_opcion(Opcion, Mensaje)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INICIO Y GESTIÓN DE PARTIDAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% iniciar_partida(+J1, +J2)
iniciar_partida(J1, J2) :-
    \+ partida_activa(_),
    assert(partida_activa(jugadores(J1, J2))),
    inicializar_jugadores([J1, J2]),
    inicializar_tablero,
    repartir_fichas([J1, J2]).

/* %NUEVO INICIAR PARTIDA (SIN PROBAR)

% filepath: c:\Users\uleon\Desktop\TRAGICERA6\PL\practicaPL\scrabble.pl
% iniciar_partida/0: Llama automáticamente a iniciar_partida/1 o iniciar_partida/2 según el modo de juego
iniciar_partida :-
    opcion(modo_juego, 'jugadorVSjugador'),
    format("Modo de juego: Jugador vs Jugador.~n"),
    iniciar_partida(_, _). % Llama a iniciar_partida/2 para dos jugadores

iniciar_partida :-
    opcion(modo_juego, 'jugadorVSmaquina'),
    format("Modo de juego: Jugador vs Máquina.~n"),
    iniciar_partida(_). % Llama a iniciar_partida/1 para un jugador

iniciar_partida :-
    format("Error: No se ha configurado el modo de juego correctamente.~n"), fail.

% iniciar_partida/1: Inicia una partida con un jugador contra la máquina
iniciar_partida(Jugador) :-
    \+ partida_activa(_),
    format("Ingrese el nombre del jugador: "),
    flush_output,
    read_line_to_string(user_input, Jugador),
    assert(partida_activa(jugadores(Jugador, maquina))),
    inicializar_jugadores([Jugador, maquina]),
    inicializar_tablero,
    repartir_fichas([Jugador, maquina]),
    format("Partida iniciada: ~w vs Máquina.~n", [Jugador]).

% iniciar_partida/2: Inicia una partida con dos jugadores
iniciar_partida(Jugador1, Jugador2) :-
    \+ partida_activa(_),
    format("Ingrese el nombre del jugador 1: "),
    flush_output,
    read_line_to_string(user_input, Jugador1),
    format("Ingrese el nombre del jugador 2: "),
    flush_output,
    read_line_to_string(user_input, Jugador2),
    assert(partida_activa(jugadores(Jugador1, Jugador2))),
    inicializar_jugadores([Jugador1, Jugador2]),
    inicializar_tablero,
    repartir_fichas([Jugador1, Jugador2]),
    format("Partida iniciada: ~w vs ~w.~n", [Jugador1, Jugador2]).

*/

% abandonar_partida(+Jugador)
abandonar_partida(J) :-
    partida_activa(jugadores(J, _)); partida_activa(jugadores(_, J)),
    retractall(partida_activa(_)),
    format("El jugador ~w ha abandonado la partida.~n", [J]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GESTIÓN DE JUGADORES Y FICHAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_jugadores(+ListaJugadores)
inicializar_jugadores([]).
inicializar_jugadores([J|R]) :-
    assert(jugador(J, 0, [])),
    inicializar_jugadores(R).

% repartir_fichas(+ListaJugadores)
repartir_fichas([]).
repartir_fichas([J|R]) :-
    generar_fichas(7, Fichas),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Fichas)),
    repartir_fichas(R).

% mostrar_fichas(+Jugador)
mostrar_fichas(J) :-
    jugador(J, _, Fichas),
    format("Fichas de ~w: ~w~n", [J, Fichas]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TABLERO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_tablero/0
inicializar_tablero :-
    retractall(casilla(_,_,_,_)),
    forall(between(1,15,F),
        forall(between(1,15,C),
            assert(casilla(F, C, libre, none)))).

% ver_tablero/0
ver_tablero :-
    forall(between(1, 15, F), (
        forall(between(1, 15, C), (
            (casilla(F, C, libre, _) -> write('.'); casilla(F, C, ocupada, Letra), write(Letra)),
            write(' ')
        )), nl)).

% colocar_letra(+Fila, +Columna, +Letra)
colocar_letra(F, C, L) :-
    casilla(F, C, libre, none),
    retract(casilla(F, C, libre, none)),
    assert(casilla(F, C, ocupada, L)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FORMAR PALABRA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% formar_palabra(+Jugador, +Orientacion, +Fila, +Columna, +Palabra)
formar_palabra(J, O, F, C, P) :-
    partida_activa(_),
    jugador(J, _, Fichas),
    atom_chars(P, Letras),
    puede_colocar(P, O, F, C),
    contiene_fichas(Fichas, Letras),
    colocar_palabra(Letras, O, F, C),
    actualizar_fichas_jugador(J, Letras),
    sumar_puntos(J, Letras, O, F, C),
    reponer_fichas(J).

% puede_colocar(+Palabra, +Orientacion, +Fila, +Columna)
puede_colocar(P, horizontal, F, C) :-
    atom_length(P, L),
    C2 is C + L - 1,
    C2 =< 15,
    L1 is L -1,
    forall(between(0, L1, I), (
        CPos is C + I,
        casilla(F, CPos, libre, _) ; casilla(F, CPos, ocupada, _)
    )).
puede_colocar(P, vertical, F, C) :-
    atom_length(P, L),
    F2 is F + L - 1,
    F2 =< 15,
    L1 is L -1,
    forall(between(0, L1, I), (
        FPos is F + I,
        casilla(FPos, C, libre, _) ; casilla(FPos, C, ocupada, _)
    )).

% contiene_fichas(+FichasJugador, +LetrasPalabra)
contiene_fichas(FJ, Ls) :-
    msort(FJ, S1), msort(Ls, S2), sublista(S2, S1).

% sublista(S, L): S está contenida en L
sublista([], _).
sublista([X|Xs], [X|Ys]) :- sublista(Xs, Ys).
sublista(Xs, [_|Ys]) :- sublista(Xs, Ys).

% colocar_palabra(+Letras, +O, +F, +C)
colocar_palabra([], _, _, _).
colocar_palabra([L|Ls], horizontal, F, C) :-
    colocar_letra(F, C, L),
    C1 is C + 1,
    colocar_palabra(Ls, horizontal, F, C1).
colocar_palabra([L|Ls], vertical, F, C) :-
    colocar_letra(F, C, L),
    F1 is F + 1,
    colocar_palabra(Ls, vertical, F1, C).

% actualizar_fichas_jugador(+J, +LetrasUsadas)
actualizar_fichas_jugador(J, Letras) :-
    jugador(J, P, F),
    remove_letras(F, Letras, FR),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, FR)).

% remove_letras(+FichasJugador, +LetrasUsadas, -Restantes)
remove_letras(F, [], F).
remove_letras(F, [L|Ls], R) :-
    select(L, F, F1),
    remove_letras(F1, Ls, R).

% sumar_puntos(+J, +Letras, +O, +F, +C)
sumar_puntos(J, Letras, _, _, _) :-
    length(Letras, N),
    Puntos is N * 1, % Simulación: 1 punto por letra
    jugador(J, Prev, Fichas),
    retract(jugador(J, Prev, Fichas)),
    Nuevo is Prev + Puntos,
    assert(jugador(J, Nuevo, Fichas)),
    format("~w ha sumado ~d puntos.~n", [J, Puntos]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ABANDONAR PARTIDA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% abandonar_partida(+Jugador)
% abandonar_partida(+Jugador)
abandonar_partida(J) :-
    (   partida_activa(jugadores(J, Otro)) ; partida_activa(jugadores(Otro, J))
    ->  format("El jugador ~w ha abandonado la partida. ~w gana.~n", [J, Otro]),
        retractall(partida_activa(_)),
        retractall(jugador(_, _, _)),
        retractall(casilla(_, _, _, _)),
        retractall(ficha_disponible(_, _))
    ;   partida_activa(_)
    ->  format("Error: el jugador ~w no participa en la partida actual.~n", [J]), fail
    ;   format("Error: no hay ninguna partida activa.~n"), fail
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FUNCIONES AUXILIARES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reponer_fichas(+Jugador)
reponer_fichas(J) :-
    jugador(J, P, FichasAct),
    length(FichasAct, N),
    M is 7 - N,
    total_fichas_disponibles(Bolsa),
    min(M, Bolsa, CantidadAReponer),
    generar_fichas(CantidadAReponer, Nuevas),
    append(FichasAct, Nuevas, Final),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Final)).

% total_fichas_disponibles(-N)
total_fichas_disponibles(N) :-
    findall(L, ficha_disponible(L, _), Ls),
    length(Ls, N).

% min/3 para obtener el mínimo entre dos valores
min(A, B, A) :- A =< B, !.
min(_, B, B).

% actualizar generar_fichas para usar la bolsa
% generar_fichas(+N, -Fichas)
generar_fichas(0, []).
generar_fichas(N, [L|R]) :-
    N > 0,
    findall(F, ficha_disponible(F, _), Bolsa),
    Bolsa \= [],
    random_member(L, Bolsa),
    retract(ficha_disponible(L, _)),
    N1 is N - 1,
    generar_fichas(N1, R).

reset_juego :- 
    retractall(partida_activa(_)),
    retractall(jugador(_, _, _)),
    retractall(casilla(_, _, _, _)),
    retractall(ficha_disponible(_, _)).


% letras_disponibles/1 inicializa la bolsa solo si está vacía
generar_fichas(0, []).
generar_fichas(N, [L|R]) :-
    N > 0,
    letras_disponibles(Letras),
    random_member(L, Letras),
    N1 is N - 1,
    generar_fichas(N1, R).

% letras_disponibles(-Lista)
letras_disponibles(['A','B','C','D','E','F','G','H','I','J','K','L','M',
                    'N','Ñ','O','P','Q','R','S','T','U','V','W','X','Y','Z','_']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% A COMPLETAR POSTERIORMENTE:
%% formar_palabra/5
%% mostrar_puntuación/0
%% ver_resumen/0
%% ver_historial/1
%% ver_ranking/0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
