% Configurar la codificación de entrada y salida como UTF-8
:- set_prolog_flag(encoding, utf8).

test_tildes :-
    format("Prueba de caracteres: á, é, í, ó, ú, ñ, ü.~n").

:- dynamic opcion/2.  % Es una estructura en la que se guarda los parametros de configuracion con su valor
:- dynamic jugador/3.
:- dynamic partida_activa/1.
:- dynamic casilla/4.
:- dynamic ficha_disponible/2.
:- dynamic historial/4.
:- dynamic turno_actual/1.
:- dynamic ultimo_iniciador/1. % Para el modo alterno

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CONFIGURACIÓN
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% opcion_valida(+Opcion, -ValoresPosibles)
opcion_valida(idioma, ['es', 'eus', 'en']).
opcion_valida(modo_juego, ['jugadorVSjugador', 'jugadorVSmaquina']).
opcion_valida(reparto_fichas, ['aleatorio', 'manual']).
opcion_valida(modo_inicio, ['normal', 'alterno']).

% ver_opcion(+Opcion)
% Dado el nombre de una opcion(Idioma,Modo_juego,modo_reparto,modo_inicio), nos da su valor
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



% Todas las ociones validas para cada parametro de configuracion
opcion_valida(idioma, castellano).
opcion_valida(idioma, euskera).
opcion_valida(idioma, ingles).
opcion_valida(modo_juego, persona_vs_maquina).
opcion_valida(modo_juego, persona_vs_persona).
opcion_valida(reparto_fichas, aleatorio).
opcion_valida(reparto_fichas, manual).
opcion_valida(inicio_partida, normal).
opcion_valida(inicio_partida, alterno).

% valores por defecto
:- dynamic opcion_inicializada/0.
:- initialization(init_config).

init_config :-
    opcion_inicializada, !.
init_config :-
    assert(opcion(idioma, castellano)),
    assert(opcion(modo_juego, persona_vs_persona)),
    assert(opcion(reparto_fichas, aleatorio)),
    assert(opcion(inicio_partida, normal)),
    assert(opcion_inicializada).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DICCIONARIO SEGÚN IDIOMA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cargar_diccionario/0 - Carga todas las palabras del idioma seleccionado
cargar_diccionario :-
    opcion(idioma, Idioma),
    ruta_diccionario(Idioma, ArchivoRelativo),
    source_file(cargar_diccionario, RutaFuente),
    file_directory_name(RutaFuente, Dir),
    atomic_list_concat([Dir, '/', ArchivoRelativo], RutaCompleta),
    retractall(palabra_valida(_)),
    open(RutaCompleta, read, S),
    leer_palabras(S),
    close(S).


leer_palabras(S) :-
    read_line_to_string(S, Linea),
    (   Linea \= end_of_file
    ->  string_upper(Linea, Mayus),
        assert(palabra_valida(Mayus)),
        leer_palabras(S)
    ;   true).

% ruta_diccionario(+Idioma, -Ruta)
ruta_diccionario(castellano, 'palabras_castellano.txt').
ruta_diccionario(euskera,    'palabras_euskera.txt').
ruta_diccionario(ingles,     'palabras_ingles.txt').

% palabra_valida(+Palabra) dinámico cargado desde fichero

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INICIO Y GESTIÓN DE PARTIDAS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% iniciar_partida/0: Llama automáticamente a iniciar_partida/1 o iniciar_partida/2 según el modo de juego
iniciar_partida :-
    opcion(modo_juego, 'jugadorVSjugador'),
    !,
    format("Modo de juego: Jugador vs Jugador.~n"),
    format("Ingrese el nombre del jugador 1: "),
    flush_output,
    read_line_to_string(user_input, Jugador1String),
    atom_string(Jugador1, Jugador1String), % Convertir a átomo
    format("Ingrese el nombre del jugador 2: "),
    flush_output,
    read_line_to_string(user_input, Jugador2String),
    atom_string(Jugador2, Jugador2String), % Convertir a átomo
    (   iniciar_partida(Jugador1, Jugador2)
    ->  true
    ;   !, fail % Detiene el flujo si iniciar_partida/2 falla
    ).

iniciar_partida :-
    opcion(modo_juego, 'jugadorVSmaquina'),
    !,
    format("Modo de juego: Jugador vs Máquina.~n"),
    format("Ingrese el nombre del jugador: "),
    flush_output,
    read_line_to_string(user_input, JugadorString),
    atom_string(Jugador, JugadorString), % Convertir a átomo
    (   iniciar_partida(Jugador)
    ->  true
    ;   !, fail % Detiene el flujo si iniciar_partida/1 falla
    ).

iniciar_partida :-
    format("Error: No se ha configurado el modo de juego correctamente.~n"), fail.

% iniciar_partida/1: Inicia una partida con un jugador contra la máquina
iniciar_partida(Jugador) :-
    nonvar(Jugador),
    opcion(modo_juego, 'jugadorVSmaquina'), % Verifica que el modo de juego sea correcto
    \+ partida_activa(_),
    assert(partida_activa(jugadores(Jugador, maquina))),
    inicializar_jugadores([Jugador, maquina]),
    inicializar_tablero,
    repartir_fichas([Jugador, maquina]),
    inicializar_turno([Jugador, maquina]),
    format("Partida iniciada: ~w vs Máquina.~n", [Jugador]), !.

iniciar_partida(Jugador) :-
    var(Jugador), % Si el parámetro no está instanciado, muestra un error
    format("Error: El nombre del jugador debe estar instanciado.~n"), fail.

iniciar_partida(_) :-
    \+ opcion(modo_juego, 'jugadorVSmaquina'), % Falla si el modo no es correcto
    format("Error: El modo de juego configurado no es 'jugadorVSmaquina'.~n"), fail.

% iniciar_partida/2: Inicia una partida con dos jugadores
iniciar_partida(J1, J2) :-
    ( var(J1); var(J2) ), !,
    format("Error: Los nombres de los jugadores deben estar instanciados.~n"), fail.

iniciar_partida(_, _) :-
    \+ opcion(modo_juego, jugadorVSjugador), !, 
    format("Error: El modo de juego configurado no es 'jugadorVSjugador'.~n"), fail.

iniciar_partida(J1, J2) :-
    J1 == J2, !, 
    format("Error: Los jugadores no pueden tener el mismo nombre.~n"), fail.

iniciar_partida(J1, J2) :-
    \+ partida_activa(_),
    assert(partida_activa(jugadores(J1, J2))),
    inicializar_jugadores([J1, J2]),
    inicializar_tablero,
    repartir_fichas([J1, J2]),
    inicializar_turno([J1, J2]),
    format("Partida iniciada: ~w vs ~w.~n", [J1, J2]),
    turno_actual(T),
    format("Es el turno de ~w.~n", [T]),
    !.

% abandonar_partida(+Jugador)
abandonar_partida(_) :-
    \+ partida_activa(_),
    format("Error: no hay ninguna partida activa.~n"),
    !, fail.

abandonar_partida(J) :-
    var(J),
    format("Error: El nombre del jugador debe estar instanciado.~n"),
    !, fail.

abandonar_partida(J) :-
    \+ jugador(J, _, _),
    format("Error: El jugador ~w no existe.~n", [J]),
    !, fail.

abandonar_partida(J) :-
    partida_activa(jugadores(J, Otro)),
    format("El jugador ~w ha abandonado la partida. ~w gana automáticamente.~n", [J, Otro]),
    reset_juego,
    !.

abandonar_partida(J) :-
    partida_activa(jugadores(Otro, J)),
    format("El jugador ~w ha abandonado la partida. ~w gana automáticamente.~n", [J, Otro]),
    reset_juego,
    !.

abandonar_partida(J) :-
    format("Error: el jugador ~w no participa en la partida actual.~n", [J]),
    !, fail.

% reset_juego/0: Limpia el estado del juego, pero mantiene las opciones de configuración
reset_juego :-
    retractall(partida_activa(_)),
    retractall(jugador(_, _, _)),
    retractall(casilla(_, _, _, _)),
    retractall(ficha_disponible(_, _)),
    retractall(historial(_, _, _, _)),
    retractall(turno_actual(_)),
    format("El juego ha sido reiniciado. Puede configurar una nueva partida.~n").

% inicializar_turno(+Jugadores)
inicializar_turno([J1, _]) :-
    opcion(modo_inicio, 'normal'), % Modo normal: siempre comienza el jugador 1
    retractall(turno_actual(_)),
    assert(turno_actual(J1)),
    format("Es el turno de ~w.~n", [J1]).

inicializar_turno([J1, J2]) :-
    opcion(modo_inicio, 'alterno'), % Modo alterno: alterna el jugador inicial
    (   retract(ultimo_iniciador(J1)) % Si el último iniciador fue J1, ahora comienza J2
    ->  assert(turno_actual(J2)),
        assert(ultimo_iniciador(J2))
    ;   retract(ultimo_iniciador(J2)) % Si el último iniciador fue J2, ahora comienza J1
    ->  assert(turno_actual(J1)),
        assert(ultimo_iniciador(J1))
    ;   % Primera partida: comienza el jugador 1
        assert(turno_actual(J1)),
        assert(ultimo_iniciador(J1))
    ).

% cambiar_turno/0: Alterna el turno entre los jugadores
cambiar_turno :-
    turno_actual(JugadorActual),
    partida_activa(jugadores(Jugador1, Jugador2)),
    (   JugadorActual == Jugador1
    ->  NuevoTurno = Jugador2
    ;   NuevoTurno = Jugador1
    ),
    retract(turno_actual(_)),
    assert(turno_actual(NuevoTurno)),
    format("Es el turno de ~w.~n", [NuevoTurno]).

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
%% mostrar_puntuación/0
%% ver_resumen/0
%% ver_historial/1
%% ver_ranking/0
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
