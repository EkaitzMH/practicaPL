% scrabble.pl - Plantilla inicial del juego estilo Scrabble en Prolog

:- dynamic opcion/2.
:- dynamic jugador/3.
:- dynamic partida_activa/1.
:- dynamic casilla/4.
:- dynamic ficha_disponible/2.
:- dynamic historial/4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONFIGURACIÓN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ver_opcion(+Opcion)
ver_opcion(O) :-
    opcion(O, V),
    format("~w: ~w~n", [O, V]).

% establecer_opcion(+Opcion, +Valor)
establecer_opcion(O, V) :-
    \+ partida_activa(_),
    (   retract(opcion(O, _)) ; true ),
    assert(opcion(O, V)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INICIO Y GESTIÓN DE PARTIDAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% iniciar_partida(+J1, +J2)
iniciar_partida(J1, J2) :-
    \+ partida_activa(_),
    assert(partida_activa(jugadores(J1, J2))),
    inicializar_jugadores([J1, J2]),
    repartir_fichas([J1, J2]).

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
%% FUNCIONES AUXILIARES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% generar_fichas(+N, -Fichas)
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
