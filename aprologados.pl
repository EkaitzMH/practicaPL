%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% APROLOGADOS: PRÁCTICA.
%% GRUPO 6: EKAITZ MURILLO, UNAI LEÓN.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SIN COMPLETAR:
%% Modo jugador VS máquina. Puede iniciarse la partida en este modo pero no está implementado.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Para configurar la codificación de entrada y salida como UTF-8
:- set_prolog_flag(encoding, utf8).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados dinámicos
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% opcion(Opcion, Valor)
:- dynamic opcion/2. % Para guardar las opciones de configuración.

% jugador (Nombre, Puntuacion, Fichas). 
:- dynamic jugador/3. % Representa cada jugador.

% partida_activa(jugadores(Nombre1, Nombre2))
:- dynamic partida_activa/1. % Representa si hay una partida activa y los jugadores que participan.

% casilla(Fila, Columna, Estado, Letra)
:- dynamic casilla/4. % Representación de cada casilla del tablero. El Estado puede ser 'libre' u 'ocupada'; y en el caso de estar ocupada se guarda la Letra que la ocupa.

% ficha_disponible(Letra, Cantidad)
:- dynamic ficha_disponible/2. % Para cada ficha de la bolsa, guarda la Letra y la Cantidad de fichas que quedan de esa letra.

% turno_actual(Jugador)
:- dynamic turno_actual/1. % Guarda el nombre del jugador que tiene el turno.

% ultimo_iniciador(Jugador)
:- dynamic ultimo_iniciador/1. % Quién inició la última partida, para el modo de turno alterno.

% jugada(Jugador, Palabra, Puntos, Casillas, FichasRestantes)
:- dynamic jugada/5. % Guarda cada jugada que realiza un jugador.

% turnos_sin_jugar(Numero)
:- dynamic turnos_sin_jugar/1. % Guarda el numero de turnos sin jugar seguidos, para finalizar la partida si ambos jugadores pasan el turno.

% palabra_valida(Palabra)
:- dynamic palabra_valida/1. % Guarda todas las palabras válidas del idioma seleccionado.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de configuración
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% opcion_valida(+Opcion, -ValoresPosibles): tiene éxito si ValoresPosibles es una lista que tiene los valores válidos para la opcion Opcion.
opcion_valida(idioma, ['es', 'eus', 'en']).
opcion_valida(modo_juego, ['jugadorVSjugador', 'jugadorVSmaquina']).
opcion_valida(reparto_fichas, ['aleatorio', 'manual']).
opcion_valida(modo_inicio, ['normal', 'alterno']).

% ver_opcion(+Opcion): siempre tiene éxito, imprime pr consola el valor de Opcion.
ver_opcion(O) :-
    opcion(O, V),
    format("~w: ~w~n", [O, V]).

% establecer_opcion(+Opcion, +Valor): tiene éxito si no hay una partida activa, Opción es una opción válida y Valor es un valor válido para esa opción.
establecer_opcion(O, V) :-
    \+ partida_activa(_),

    opcion_valida(O, ValoresPosibles),
    (   member(V, ValoresPosibles)  
    ->  (retract(opcion(O, _)) ; true),
        assert(opcion(O, V))
    ;   format("Error: ~w no es un valor válido para la opción ~w.~n", [V, O]), fail
    ).

% configurar_opciones: siempre tiene éxito, es un predicado auxiliar para facilitar la configuración de las opciones. Pregunta por consola los valores y el usuario debe introducirlos.
configurar_opciones :-
    preguntar_opcion(idioma, "Seleccione el idioma (es/eus/en): "),
    preguntar_opcion(modo_juego, "Seleccione el modo de juego (jugadorVSjugador/jugadorVSmaquina): "),
    preguntar_opcion(reparto_fichas, "Seleccione el reparto de fichas (aleatorio/manual): "),
    preguntar_opcion(modo_inicio, "Seleccione el modo de inicio (normal/alterno): ").

% preguntar_opcion(+Opcion, +Mensaje): siempre tiene éxito, pregunta por consola con el texto Mensaje el valor para la opción Opcion y la establece.
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

% Valores por defecto y para la configuración inicial
:- dynamic opcion_inicializada/0.
:- initialization(init_config).

init_config :-
    opcion_inicializada, !.
init_config :-
    assert(opcion(idioma, es)),
    assert(opcion(modo_juego, jugadorVSjugador)),
    assert(opcion(reparto_fichas, aleatorio)),
    assert(opcion(modo_inicio, normal)),
    assert(turnos_sin_jugar(0)),
    assert(opcion_inicializada).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicado para cargar el diccionario segun el idioma
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% cargar_diccionario: siempre tiene éxito, carga todas las palabras del idioma seleccionado.
% NOTA: el diccionario se encuentra en la carpeta del proyecto.
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

% leer_palabras(+Stream): siempre tiene éxito, lee el diccionario de palabras y lo almacena en predicados dinámicos.
leer_palabras(S) :-
    read_line_to_string(S, Linea),
    (   Linea \= end_of_file
    ->  string_upper(Linea, Mayus),
        assert(palabra_valida(Mayus)),
        leer_palabras(S)
    ;   true).

% ruta_diccionario(+Idioma, -Ruta): tiene éxito si Ruta es el nombre del archivo del diccionario del idioma Idioma
ruta_diccionario(es,   'palabras_castellano_sin_tildes.txt').
ruta_diccionario(eus,  'palabras_euskera.txt').
ruta_diccionario(en,   'palabras_ingles.txt').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de inicio de partida
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% iniciar_partida: predicado auxiliar para iniciar la partida de forma más sencilla. Pregunta el nombre del / los jugador/es e inicia la partida de un jugador o de dos jugadores segun la configuracion escogida.
% Si no hay ninguna configuracion escogida se lanza un error y se pide ejecutar el predicado configurar_opciones.
iniciar_partida :-
    opcion(modo_juego, 'jugadorVSjugador'),
    !,
    format("Modo de juego: Jugador vs Jugador.~n"),
    format("Ingrese el nombre del jugador 1: "),
    flush_output,
    read_line_to_string(user_input, Jugador1String),
    atom_string(Jugador1, Jugador1String), 
    format("Ingrese el nombre del jugador 2: "),
    flush_output,
    read_line_to_string(user_input, Jugador2String),
    atom_string(Jugador2, Jugador2String), 
    (   iniciar_partida(Jugador1, Jugador2)
    ->  true
    ;   !, fail 
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
    format("Error: No se ha configurado el modo de juego correctamente. Ejecute el predicado configurar_opciones ~n"), fail.

% iniciar_partida(Jugador): tiene éxito si el Jugador está instanciado y no hay una partida activa. Inicia una partida con el jugador Jugador y la máquina.
iniciar_partida(Jugador) :-
    nonvar(Jugador),
    opcion(modo_juego, 'jugadorVSmaquina'), 
    \+ partida_activa(_),
    assert(partida_activa(jugadores(Jugador, maquina))),
    inicializar_jugadores([Jugador, maquina]),
    format("Cargando...\n"),
    inicializar_tablero,
    cargar_diccionario,
    inicializar_bolsa,
    (   opcion(reparto_fichas, aleatorio)
        -> repartir_fichas([Jugador, maquina])
        ; true
    ),
    inicializar_turno([Jugador, maquina]),
    format("Partida iniciada: ~w vs Máquina.~n", [Jugador]), !.

% iniciar_partida(Jugador1, Jugador2): tiene éxito si ambos Jugadores están instanciados, no son iguales y no hay una partida activa. Inicia una partida entre los dos jugadores.
iniciar_partida(J1, J2) :-
    J1 == J2, !, 
    format("Error: Los jugadores no pueden tener el mismo nombre.~n"), fail.

iniciar_partida(J1, J2) :-
    \+ partida_activa(_),
    assert(partida_activa(jugadores(J1, J2))),
    inicializar_jugadores([J1, J2]),
    format("Cargando...\n"),
    inicializar_tablero,
    cargar_diccionario,
    inicializar_bolsa,
    (   opcion(reparto_fichas, aleatorio)
        -> repartir_fichas([J1, J2])
        ; true
    ),
    inicializar_turno([J1, J2]),
    turno_actual(T),
    format("Partida iniciada: ~w vs ~w.~n", [J1, J2]),
    format("Es el turno de ~w.~n", [T]),
    !.

% abandonar_partida(+Jugador): tiene éxito si hay una partida activa y el jugador Jugador este participando en ella. Se retirará Jugador de la partida y se le otorgará la victoria al otro jugador. Despues se reiniciará el juego.
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
      jugador(J, P1, _),
    jugador(Otro, P2, _),
    actualizar_historial(Otro, victoria, P2),
    actualizar_historial(J, derrota, P1),
    reset_juego,
    !.

abandonar_partida(J) :-
    partida_activa(jugadores(Otro, J)),
    format("El jugador ~w ha abandonado la partida. ~w gana automáticamente.~n", [J, Otro]),
    jugador(J, P1, _),
    jugador(Otro, P2, _),
    actualizar_historial(Otro, victoria, P2),
    actualizar_historial(J, derrota, P1),
    reset_juego,
    !.

abandonar_partida(J) :-
    format("Error: el jugador ~w no participa en la partida actual.~n", [J]),
    !, fail.

% reset_juego: siempre tiene éxito. Termina la partida actual borrando los dats de la partida, pero deja las opciones de configuración.
reset_juego :-
    retractall(partida_activa(_)),
    retractall(jugador(_, _, _)),
    retractall(casilla(_, _, _, _)),
    retractall(ficha_disponible(_, _)),
    retractall(historial(_, _, _, _)),
    retractall(turno_actual(_)),
    retractall(jugada(_, _, _, _, _)),
    format("El juego ha sido reiniciado. Puede configurar una nueva partida.~n").

% inicializar_turno(+Jugadores): siempre tiene éxito, inicializa el turno del jugador que comienza la partida dependiendo del modo de inicio.

% Modo normal: el jugador 1 comienza
inicializar_turno([J1, _]) :-
    opcion(modo_inicio, 'normal'), %
    retractall(turno_actual(_)),
    assert(turno_actual(J1)).

% Modo alterno: el jugador que no ha comenzado la partida anterior comienza
inicializar_turno([J1, J2]) :-
    opcion(modo_inicio, 'alterno'), 
    (   retract(ultimo_iniciador(J1)) 
    ->  assert(turno_actual(J2)),
        assert(ultimo_iniciador(J2))
    ;   retract(ultimo_iniciador(J2)) 
    ->  assert(turno_actual(J1)),
        assert(ultimo_iniciador(J1))
    ;  
        assert(turno_actual(J1)),
        assert(ultimo_iniciador(J1))
    ).

% cambiar_turno: siempre tiene éxito, cambia el turno al otro jugador.
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

% pasar_turno(+Jugador): tiene éxito si el jugador Jugador tiene el turno. Se lo pasa al otro jugador y se aumenta el contador de turnos sin jugar. Si ambos jugadores han pasado el turno, se finaliza la partida.
pasar_turno(J) :-
    (   \+ partida_activa(_)
    ->  format("Error: No hay una partida activa.~n"), fail
    ;   true
    ),
    (   turno_actual(J)
    ->  cambiar_turno,
        format("El turno ha pasado al siguiente jugador.~n")
    ;   format("Error: No es el turno del jugador ~w.~n", [J]), fail
    ),
    retract(turnos_sin_jugar(N)),
    N1 is N + 1,
    assert(turnos_sin_jugar(N1)),
    ( N1 >= 2
    -> format("Ambos jugadores han pasado el turno."), finalizar_partida
    ;  true
    ).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de jugadores y fichas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_jugadores(+ListaJugadores): siempre tiene éxito, inicializa los jugadores con su nombre, 0 puntos y una lista vacia donde se guardarán las fichas.
inicializar_jugadores([]).
inicializar_jugadores([J|R]) :-
    assert(jugador(J, 0, [])),
    inicializar_jugadores(R).

% repartir_fichas(+ListaJugadores): siempre tiene éxito, reparte 7 fichas aleatorias a cada jugador de la lista ListaJugadores.
repartir_fichas([]).
repartir_fichas([J|R]) :-
    jugador(J, P, _),
    generar_fichas(7, Fichas),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Fichas)),
    repartir_fichas(R).

% mostrar_fichas(+Jugador): tiene éxito si el Jugador existe, y muestra sus fichas por consola.
mostrar_fichas(J) :-
    jugador(J, _, Fichas),
    format("Fichas de ~w: ~w~n", [J, Fichas]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de la bolsa de fichas por idioma
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_bolsa: siempre tiene éxito, prepara la bolsa de fichas según el idioma actual.
inicializar_bolsa :-
    retractall(ficha_disponible(_, _)),
    opcion(idioma, Idioma),
    fichas_por_idioma(Idioma, Distribucion),
    agregar_fichas(Distribucion).

% fichas_por_idioma(+Idioma, -Distribucion): tiene éxito si Distribucion es la lista de letras y su cantidad en el idioma Idioma.
% Cada elemento de Distribucion es un par con la letra y cantidad de fichas que tiene en la bolsa.
fichas_por_idioma(es, [(a, 12), (e, 12), (o, 9), (s, 6), (r, 5), (n, 5), (l, 4), (d, 3), (t, 4), (u, 5), 
                       (i, 6), (c, 4), (m, 2), (p, 2), (b, 2), (g, 2), (v, 1), (h, 2), (f, 1), (y, 1), 
                       (j, 1), ('ñ', 1), (q, 1), (z, 1), (x, 1)]).

fichas_por_idioma(eus, [(a, 14), (e, 12), (i, 9), (o, 6), (u, 6), (n, 8), (d, 4), (t, 8), (l, 2), (r, 7), 
                        (k, 5), (g, 2), (b, 2), (z, 2), (m, 1), (s, 2), (h, 2), (p, 1), (x, 1), (j, 1), 
                        (f, 1)]).

fichas_por_idioma(en, [(e, 12), (a, 9), (i, 9), (o, 8), (n, 6), (r, 6), (t, 6), (l, 4), (s, 4), (u, 4), 
                       (d, 4), (g, 3), (b, 2), (c, 2), (m, 2), (p, 2), (f, 2), (h, 2), (v, 2), (w, 2), 
                       (y, 2), (k, 1), (j, 1), (x, 1), (q, 1), (z, 1)]).

% agregar_fichas(+ListaLetraCantidad): siempre tiene éxito, dado un elemento de Distribucion (un par con la letra y la cantidad de repeticiones) agrega la letra y su cantidad a la bolsa de fichas.
agregar_fichas([]) :- !.
agregar_fichas([(Letra, Cant) | Resto]) :-
    assert(ficha_disponible(Letra, Cant)), 
    agregar_fichas(Resto).

% ver_bolsa: siempre tiene éxito, muestra por consola la lista de letras disponibles en la bolsa y su cantidad.
ver_bolsa :-
    findall(L-C, ficha_disponible(L, C), Fichas),
    imprimir_bolsa(Fichas).

% imprimir_bolsa(+ListaLetraCantidad): predicado auxiliar para ver_bolsa, siempre tiene éxito. Imprime por consola la lista de letras y su cantidad.
imprimir_bolsa([]).
imprimir_bolsa([L-C|R]) :-
    format("~w: ~d~n", [L, C]),
    imprimir_bolsa(R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de tablero
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% casilla_especial(+Fila, +Columna, ?Tipo, ?Multiplicador): tiene exito si la casilla correspondiente a Fila y Columna es una casilla que multiplica Tipo (letra o palabra) por Multiplicador.
casilla_especial(1, 1, palabra, 3).
casilla_especial(1, 8, palabra, 3).
casilla_especial(1, 15, palabra, 3).
casilla_especial(8, 8, palabra, 2).
casilla_especial(8, 15, palabra, 3).
casilla_especial(8, 1, palabra, 3).
casilla_especial(15, 1, palabra, 3).
casilla_especial(15, 8, palabra, 3).
casilla_especial(15, 15, palabra, 3).
casilla_especial(1, 4, letra, 2).
casilla_especial(1, 12, letra, 2).
casilla_especial(3, 7, letra, 2).
casilla_especial(3, 9, letra, 2).
casilla_especial(4, 1, letra, 2).
casilla_especial(4, 8, letra, 2).
casilla_especial(4, 15, letra, 2).
casilla_especial(7, 3, letra, 2).
casilla_especial(7, 7, letra, 2).
casilla_especial(7, 9, letra, 2).
casilla_especial(7, 13, letra, 2).
casilla_especial(8, 4, letra, 2).
casilla_especial(8, 12, letra, 2).
casilla_especial(9, 3, letra, 2).
casilla_especial(9, 7, letra, 2).
casilla_especial(9, 9, letra, 2).
casilla_especial(9, 13, letra, 2).
casilla_especial(12, 1, letra, 2).
casilla_especial(12, 8, letra, 2).
casilla_especial(12, 15, letra, 2).
casilla_especial(13, 7, letra, 2).
casilla_especial(13, 9, letra, 2).
casilla_especial(12, 4, letra, 2).
casilla_especial(12, 12, letra, 2).
casilla_especial(2, 2, palabra, 2).
casilla_especial(2, 14, palabra, 2).
casilla_especial(14, 2, palabra, 2).
casilla_especial(14, 14, palabra, 2).
casilla_especial(3, 3, palabra, 2).
casilla_especial(3, 13, palabra, 2).
casilla_especial(13, 3, palabra, 2).
casilla_especial(13, 13, palabra, 2).
casilla_especial(4, 4, palabra, 2).
casilla_especial(4, 12, palabra, 2).
casilla_especial(12, 4, palabra, 2).
casilla_especial(12, 12, palabra, 2).
casilla_especial(5, 5, palabra, 2).
casilla_especial(5, 11, palabra, 2).
casilla_especial(11, 5, palabra, 2).
casilla_especial(11, 11, palabra, 2).
casilla_especial(2, 6, letra, 3).
casilla_especial(2, 10, letra, 3).
casilla_especial(14, 6, letra, 3).
casilla_especial(14, 10, letra, 3).
casilla_especial(6, 2, letra, 3).
casilla_especial(6, 6, letra, 3).
casilla_especial(6, 10, letra, 3).
casilla_especial(6, 14, letra, 3).
casilla_especial(10, 2, letra, 3).
casilla_especial(10, 6, letra, 3).
casilla_especial(10, 10, letra, 3).
casilla_especial(10, 14, letra, 3).

% inicializar_tablero: siempre tiene éxito, inicializa el tablero guardando las casillas como predicados dinámicos.
inicializar_tablero :-
    retractall(casilla(_,_,_,_)),
    forall(between(1,15,F),
        forall(between(1,15,C),
            assert(casilla(F, C, libre, none)))).

% ver_tablero: siempre tiene éxito, muestra el tablero por consola. Si la casilla está ocupada, muestra la letra que ocupa la casilla. Si es una casilla especial, muestra el tipo de multiplicador.
ver_tablero :-
    write('    '), 
    forall(between(1, 15, C), (format("~|~`0t~d~2+", [C]), write('  '))), nl, 
    write('   '),
    forall(between(1, 15, _), write('----')), nl, 
    forall(between(1, 15, F), (
        format("~|~`0t~d~2+ |", [F]), 
        forall(between(1, 15, C), (
            (casilla(F, C, ocupada, Letra) ->
    format(" ~w ", [Letra])
;
    (casilla_especial(F, C, letra, 2) -> write('L*2');
     casilla_especial(F, C, letra, 3) -> write('L*3');
     casilla_especial(F, C, palabra, 2) -> write('P*2');
     casilla_especial(F, C, palabra, 3) -> write('P*3');
     write('   ')
    )
),
            
            write('|') 
        )),
        nl,
        write('   '),
        forall(between(1, 15, _), write('----')), nl 
    )).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de puntuaciones
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% valor_letra(+Letra, +Idioma, -Puntos): tiene exito si Puntos es el valor de la letra Letra en el idioma Idioma.
valor_letra(a, es, 1) :- !.
valor_letra(e, es, 1) :- !.
valor_letra(i, es, 1) :- !.
valor_letra(l, es, 1) :- !.
valor_letra(n, es, 1) :- !.
valor_letra(o, es, 1) :- !.
valor_letra(r, es, 1) :- !.
valor_letra(s, es, 1) :- !.
valor_letra(t, es, 1) :- !.
valor_letra(u, es, 1) :- !.
valor_letra(d, es, 2) :- !.
valor_letra(g, es, 2) :- !.
valor_letra(b, es, 3) :- !.
valor_letra(c, es, 3) :- !.
valor_letra(m, es, 3) :- !.
valor_letra(p, es, 3) :- !.
valor_letra(f, es, 4) :- !.
valor_letra(h, es, 4) :- !.
valor_letra(v, es, 4) :- !.
valor_letra(y, es, 4) :- !.
valor_letra(q, es, 5) :- !.
valor_letra(j, es, 8) :- !.
valor_letra(x, es, 8) :- !.
valor_letra('ñ', es, 8) :- !.
valor_letra(z, es, 10) :- !.

valor_letra(a, eus, 1) :- !.
valor_letra(e, eus, 1) :- !.
valor_letra(i, eus, 1) :- !.
valor_letra(n, eus, 1) :- !.
valor_letra(o, eus, 1) :- !.
valor_letra(t, eus, 1) :- !.
valor_letra(u, eus, 1) :- !.
valor_letra(r, eus, 2) :- !.
valor_letra(k, eus, 2) :- !.
valor_letra(d, eus, 3) :- !.
valor_letra(b, eus, 4) :- !.
valor_letra(z, eus, 4) :- !.
valor_letra(l, eus, 5) :- !.
valor_letra(g, eus, 5) :- !.
valor_letra(h, eus, 5) :- !.
valor_letra(s, eus, 5) :- !.
valor_letra(j, eus, 8) :- !.
valor_letra(m, eus, 8) :- !.
valor_letra(p, eus, 8) :- !.
valor_letra(f, eus, 10) :- !.
valor_letra(x, eus, 10) :- !.

valor_letra(a, en, 1) :- !.
valor_letra(e, en, 1) :- !.
valor_letra(i, en, 1) :- !.
valor_letra(l, en, 1) :- !.
valor_letra(n, en, 1) :- !.
valor_letra(o, en, 1) :- !.
valor_letra(r, en, 1) :- !.
valor_letra(s, en, 1) :- !.
valor_letra(t, en, 1) :- !.
valor_letra(u, en, 1) :- !.
valor_letra(d, en, 2) :- !.
valor_letra(g, en, 2) :- !.
valor_letra(b, en, 3) :- !.
valor_letra(c, en, 3) :- !.
valor_letra(m, en, 3) :- !.
valor_letra(p, en, 3) :- !.
valor_letra(f, en, 4) :- !.
valor_letra(h, en, 4) :- !.
valor_letra(v, en, 4) :- !.
valor_letra(w, en, 4) :- !.
valor_letra(y, en, 4) :- !.
valor_letra(k, en, 5) :- !.
valor_letra(j, en, 8) :- !.
valor_letra(x, en, 8) :- !.
valor_letra(q, en, 10) :- !.
valor_letra(z, en, 10) :- !.

% sumar_puntos(+Jugador, +Letras, +Orientacion, +Fila, +Columna): suma al jugador Jugador los puntos que da la palabra que forma la lista Letras comenzando en la posicion (Fila,Columna) en la orientación Orientacion.
sumar_puntos(J, Letras, O, F, C) :-
    opcion(idioma, Idioma),
    puntuaje_palabra(Letras, Idioma, O, F, C, Puntos),
    jugador(J, Anterior, Fichas),
    retract(jugador(J, Anterior, Fichas)),
    Nuevo is Anterior + Puntos,
    assert(jugador(J, Nuevo, Fichas)).

% puntuaje_palabra(+Letras, +Idioma, +Orientacion, +Fila, +Columna, -Puntos): tiene exito si Puntos es el puntuaje que da la palabra que forma la lista Letras en el idioma Idioma que se encuentra en la orientacion Orientacion, empezando la posicion (Fila, Columna).
puntuaje_palabra(Letras, Idioma, O, F, C, PuntosTotales) :-
    puntuar_letras(Letras, Idioma, O, F, C, 0, 1, PuntosTotales).

% puntuar_letras(+Letras, +Idioma, +Orientacion, +Fila, +Columna, +Acum, +MultPalabra, -PuntosFinal): tiene exito si PuntosFinal es el puntuaje que da la palabra que forma la lista Letras en el idioma Idioma que se encuentra en la orientacion Orientacion empezando en la posicion (Fila, Columna). Acum es el acumulador de puntos y MultPalabra es el multiplicador de palabra.
puntuar_letras([], _, _, _, _, Acum, MultPalabra, Total) :-
    Total is Acum * MultPalabra.

puntuar_letras([L|Ls], Idioma, horizontal, F, C, Acum, MPW, Total) :-
    letra_valor(L, F, C, Idioma, ValLetra, MultPalabra),
    NuevoAcum is Acum + ValLetra,
    ( MultPalabra > 1 -> NuevoMPW is MPW * MultPalabra ; NuevoMPW = MPW ),
    C1 is C + 1,
    puntuar_letras(Ls, Idioma, horizontal, F, C1, NuevoAcum, NuevoMPW, Total).

puntuar_letras([L|Ls], Idioma, vertical, F, C, Acum, MPW, Total) :-
    letra_valor(L, F, C, Idioma, ValLetra, MultPalabra),
    NuevoAcum is Acum + ValLetra,
    ( MultPalabra > 1 -> NuevoMPW is MPW * MultPalabra ; NuevoMPW = MPW ),
    F1 is F + 1,
    puntuar_letras(Ls, Idioma, vertical, F1, C, NuevoAcum, NuevoMPW, Total).

% letra_valor(+Letra, +Fila, +Columna, +Idioma, -ValorFinal, -MultPalabra): tiene éxito si ValorFinal es el valor de la letra Letra en la posicion (Fila,Columna) en el idioma Idioma, teniendo en cuenta los multiplicadores de letra y palabra.
letra_valor(L, F, C, Idioma, ValorLetra, PMult) :-
    downcase_atom(L, Lmin),
    valor_letra(Lmin, Idioma, Base),
    (   casilla(F, C, libre, _) ->
        (   casilla_especial(F, C, letra, LMult) -> ValorLetra is Base * LMult ; ValorLetra = Base ),
        (   casilla_especial(F, C, palabra, PMult) -> true ; PMult = 1 )
    ;   ValorLetra = Base,
        PMult = 1
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados para formar las palabras
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% formar_palabra(+Jugador, +Orientacion, +Fila, +Columna, +Palabra): tiene éxito si hay una partida activa, es el turno del Jugador, la Palabra está en el diccionario, la Palabra encaja en el tablero según las casillas libres y ocupadas, la Palabra está cruzando o extendiendo a una palabra ya formada (o pasa por el centro del tablero en el caso de que no haya palabras todavía) y el Jugador tiene las fichas necesarias para formar la Palabra. Coloca la palabra en el tablero, actualiza las fichas del jugador si el modo es aleatorio, suma los puntos y guarda la jugada. Finalmente, cambia el turno al siguiente jugador.
formar_palabra(J, O, F, C, P) :-
    (   partida_activa(_)
    ->  true % La partida esta activa
    ;   format("Error: No hay una partida activa.~n"), fail
    ),
    (   turno_actual(J)
    ->  true % Es el turno del jugador
    ;   format("Error: No es el turno del jugador ~w.~n", [J]), fail
    ),
    (   jugador(J, _, Fichas)
    ->  true % El jugador está registrado
    ;   format("Error: El jugador ~w no está registrado.~n", [J]), fail
    ),
    string_upper(P, Mayus),
    (   palabra_valida(Mayus)
    ->  true % La palabra esta en el diccionario
    ;   format("Error: La palabra '~w' no está en el diccionario.~n", [P]), fail
    ),
    atom_chars(P, Letras),
    (   puede_colocar(P, O, F, C)
    ->  true % La palabra cabe en el tablero
    ;   format("Error: La palabra '~w' no puede colocarse en el tablero.~n", [P]), fail
    ),
    (   tablero_vacio % Si es el primer turno (tablero vacío)
    -> (  pasa_por_casilla_central(F, C, O, P) % La palabra tiene que pasar por el centro
          -> true
          ; format("En el primer turno, la palabra debe pasar por la casilla central (8,8)."), fail
       )
    ;  validar_enganche_o_extension(Letras, O, F, C)
    ),
    letras_en_tablero(Letras, O, F, C, LetrasEnTablero), % Obtiene las letras que ya están en el tablero
    subtract(Letras, LetrasEnTablero, LetrasUsadas), % Calcula las letras que el jugador realmente usó
    append(Fichas, LetrasEnTablero, FichasCombinadas),
    (   contiene_fichas(FichasCombinadas, Letras)
    ->  true % El jugador tiene las letras necesarias para formar la palabra
    ;   format("Error: El jugador ~w no tiene las fichas necesarias para formar la palabra '~w'.~n", [J, P]), fail
    ),
    sumar_puntos(J, Letras, O, F, C),
    colocar_palabra(Letras, O, F, C), % Coloca la palabra en el tablero
    format("La palabra '~w' ha sido colocada en el tablero.~n", [P]),
    actualizar_fichas_jugador(J, LetrasUsadas), % Solo elimina las letras realmente usadas
    jugador(J, _, FichasRestantes),
    (   opcion(reparto_fichas, aleatorio)
        -> reponer_fichas(J) % Repone las fichas del jugador
        ; true
    ),
    retractall(turnos_sin_jugar(_)),
    assert(turnos_sin_jugar(0)),
    (   opcion(idioma, Idioma),
        atom_chars(P, Letras),
        puntuaje_palabra(Letras, Idioma, O, F, C, Puntos),
        casillas_por_donde_pasa(Letras, O, F, C, Casillas),
        atom_string(P, PalabraStr),
        ground([J, PalabraStr, Puntos, FichasRestantes]),
        assertz(jugada(J, PalabraStr, Puntos, Casillas, FichasRestantes))
    ->  true ;   
    format("No se pudo guardar la jugada por falta de datos instanciados.~n")
    ),

    cambiar_turno, !.

% validar_enganche_o_extension(+Letras, +Orientacion, +FilaInicial, +ColInicial): tiene éxito si la palabra representada en la lista Letras colocada en la posición (FilaInicial, ColInicial) está cruzando o extendiendo una palabra ya formada.
validar_enganche_o_extension(Letras, O, F, C) :-
    casillas_por_donde_pasa(Letras, O, F, C, Casillas),
    % Debe tocar al menos una letra ya existente
    (   intersecta_con_casillas_previas(Casillas)
    ->  true
    ;   format("Error: La palabra debe tocar al menos una ya existente.~n"), fail
    ),
    % Si está extendiendo una palabra, debe usarla completa
    (   esta_extendiendo_palabra(Casillas, O, PalabraOriginal, CasillasOriginal)
    ->  length(CasillasOriginal, LenOriginal),
        length(PalabraOriginal, LenOriginal)
    ;   true
    ).

% intersecta_con_casillas_previas(+Casillas): tiene éxito si las Casillas intersectan con alguna casilla ya ocupada en el tablero.
intersecta_con_casillas_previas(Casillas) :-
    jugada(_, _, _, CasillasPrevias, _),
    member(Pos, Casillas),
    member(Pos, CasillasPrevias),
    !.

% esta_extendiendo_palabra(+Casillas, +Orientacion, -Palabra, -CasillasPalabra): tiene éxito si la palabra Palabra está extendiendo una palabra ya formada en el tablero. Las CasillasPalabra son las coordenadas de la palabra original.
esta_extendiendo_palabra(Casillas, Orientacion, LetrasPalabra, CasillasPalabra) :-
    jugada(_, Palabra, _, CasillasPalabra, _),
    misma_linea_y_orientacion(CasillasPalabra, Casillas, Orientacion),
    sublista_consecutiva(CasillasPalabra, Casillas),
    atom_chars(Palabra, LetrasPalabra).

% misma_linea_y_orientacion(+Casillas1, +Casillas2, +Orientacion): tiene éxito si las Casillas1 y Casillas2 están en la misma línea (fila o columna según orientación) y tienen la misma orientación (horizontal o vertical).
misma_linea_y_orientacion(Casillas1, Casillas2, horizontal) :-
    forall(member((F,_), Casillas1), member((F,_), Casillas2)).
misma_linea_y_orientacion(Casillas1, Casillas2, vertical) :-
    forall(member((_,C), Casillas1), member((_,C), Casillas2)).

% sublista_consecutiva(+Sub, +Lista): tiene éxito si Sub es una sublista consecutiva de Lista.
sublista_consecutiva(Sub, Lista) :-
    append(_, Resto, Lista),
    append(Sub, _, Resto).

% casillas_por_donde_pasa(+Palabra, +Orientacion, +FilaInicial, +ColInicial, -Casillas): tiene éxito si Casillas es una lista de coordenadas (F,C) por donde pasa la Palabra en la Orientacion dada, comenzando en la posición (FilaInicial, ColInicial).
casillas_por_donde_pasa([], _, _, _, []) :- !.
casillas_por_donde_pasa([_|Letras], horizontal, F, C, [(F, C)|Resto]) :-
    C1 is C + 1,
    casillas_por_donde_pasa(Letras, horizontal, F, C1, Resto).
casillas_por_donde_pasa([_|Letras], vertical, F, C, [(F, C)|Resto]) :-
    F1 is F + 1,
    casillas_por_donde_pasa(Letras, vertical, F1, C, Resto).

% letras_en_tablero(+Letras, +Orientacion, +Fila, +Columna, -LetrasEnTablero): tiene éxito si LetrasEnTablero es la lista de letras que ya están ocupando el tablero en las posiciones correspondientes a la Orientacion comenzando en la posición (Fila, Columna).
letras_en_tablero([], _, _, _, []). % Caso base: no hay más letras que procesar.
letras_en_tablero([L|Ls], horizontal, F, C, [L|Resto]) :-
    casilla(F, C, ocupada, L), % La casilla está ocupada y la letra coincide.
    C1 is C + 1, % Avanza a la siguiente columna.
    letras_en_tablero(Ls, horizontal, F, C1, Resto).
letras_en_tablero([L|Ls], horizontal, F, C, Resto) :-
    \+ casilla(F, C, ocupada, L), % La casilla no está ocupada con la letra correspondiente.
    C1 is C + 1, % Avanza a la siguiente columna.
    letras_en_tablero(Ls, horizontal, F, C1, Resto).
letras_en_tablero([L|Ls], vertical, F, C, [L|Resto]) :-
    casilla(F, C, ocupada, L), % La casilla está ocupada y la letra coincide.
    F1 is F + 1, % Avanza a la siguiente fila.
    letras_en_tablero(Ls, vertical, F1, C, Resto).
letras_en_tablero([L|Ls], vertical, F, C, Resto) :-
    \+ casilla(F, C, ocupada, L), % La casilla no está ocupada con la letra correspondiente.
    F1 is F + 1, % Avanza a la siguiente fila.
    letras_en_tablero(Ls, vertical, F1, C, Resto).

% tablero_vacio: tiene éxito si todas las casillas del tablero están libres.
tablero_vacio :-
    \+ casilla(_, _, ocupada, _).

% pasa_por_casilla_central(+Fila, +Columna, +Orientacion, +Palabra): tiene éxito si la palabra pasa por la casilla central (8,8).
pasa_por_casilla_central(F, C, Orientacion, Palabra) :-
    atom_length(Palabra, Longitud),
    (   Orientacion = horizontal
    ->  F = 8, % La fila debe ser 8
        8 >= C, % La casilla central está a la derecha o en la posición inicial
        C + Longitud - 1 >= 8 % La casilla central está dentro del rango de columnas ocupadas
    ;   Orientacion = vertical
    ->  C = 8, % La columna debe ser 8
        8 >= F, % La casilla central está debajo o en la posición inicial
        F + Longitud - 1 >= 8 % La casilla central está dentro del rango de filas ocupadas
    ;   fail % Si no es horizontal ni vertical, falla
    ).

% puede_colocar(+Palabra, +Orientacion, +Fila, +Columna): tiene éxito si la palabra Palabra puede colocarse en el tablero en la orientacion Orientacion comenzando en la posicion (Fila,Columna).
puede_colocar(P, horizontal, F, C) :-
    atom_chars(P, Letras), % Convierte la palabra en una lista de letras
    atom_length(P, L),
    C2 is C + L - 1,
    C2 =< 15,
    L1 is L - 1,
    forall(between(0, L1, I), (
        CPos is C + I,
        nth0(I, Letras, Letra), % Obtiene la letra correspondiente de la palabra
        (   casilla(F, CPos, libre, _) % Si la casilla está libre, es válida
        ;   casilla(F, CPos, ocupada, Letra) % Si está ocupada, verifica que la letra coincida
        )
    )).

puede_colocar(P, vertical, F, C) :-
    atom_chars(P, Letras), % Convierte la palabra en una lista de letras
    atom_length(P, L),
    F2 is F + L - 1,
    F2 =< 15,
    L1 is L - 1,
    forall(between(0, L1, I), (
        FPos is F + I,
        nth0(I, Letras, Letra), % Obtiene la letra correspondiente de la palabra
        (   casilla(FPos, C, libre, _) % Si la casilla está libre, es válida
        ;   casilla(FPos, C, ocupada, Letra) % Si está ocupada, verifica que la letra coincida
        )
    )).

% colocar_palabra(+Letras, +Orientacion, +Fila, +Columna): siempre tiene éxito, coloca la palabra que forman las letras de la lista Letras en el tablero en la orientacion Orientacion, comenzando en la posicion (Fila,Columna).
colocar_palabra([], _, _, _).
colocar_palabra([L|Ls], horizontal, F, C) :-
    colocar_letra(F, C, L),
    C1 is C + 1,
    colocar_palabra(Ls, horizontal, F, C1).

colocar_palabra([L|Ls], vertical, F, C) :-
    colocar_letra(F, C, L),
    F1 is F + 1,
    colocar_palabra(Ls, vertical, F1, C).

% colocar_letra(+Fila, +Columna, +Letra): tiene éxito si la casilla en (Fila, Columna) está libre o la letra que la ocupa coincide con Letra. Coloca Letra en dicha casilla.
colocar_letra(F, C, L) :-
    (   casilla(F, C, libre, none) % Si la casilla está libre
    ->  retract(casilla(F, C, libre, none)), % Elimina la casilla libre
        assert(casilla(F, C, ocupada, L)) % Coloca la letra en la casilla
    ;   casilla(F, C, ocupada, L) % Si la casilla ya está ocupada con la misma letra
    ->  true % No hace nada, ya está colocada correctamente
    ;   format("Error: No se puede colocar la letra ~w en la casilla (~w, ~w).~n", [L, F, C]),
        fail % Falla si la casilla está ocupada con una letra diferente
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de fichas
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% contiene_fichas(+FichasJugador, +LetrasPalabra): tiene éxito si las letras de la palabra LetrasPalabra están contenidas en las fichas del jugador FichasJugador.
contiene_fichas(FJ, Ls) :-
    msort(FJ, S1), msort(Ls, S2), sublista(S2, S1).

% sublista(S, L): tiene éxito si S es una sublista de L.
sublista([], _).
sublista([X|Xs], [X|Ys]) :- sublista(Xs, Ys).
sublista(Xs, [_|Ys]) :- sublista(Xs, Ys).

% actualizar_fichas_jugador(+Jugador, +LetrasUsadas): tiene éxito si las LetrasUsadas están entre las fichas del Jugador. Elimina las las letras de la lista LetrasUsadas de las fichas del Jugador.
actualizar_fichas_jugador(J, Letras) :-
    jugador(J, P, F),
    (   contiene_fichas(F, Letras) % Verifica que el jugador tiene las fichas necesarias
    ->  remove_letras(F, Letras, FR),
        retract(jugador(J, P, _)),
        assert(jugador(J, P, FR))
    ;   format("Error: El jugador ~w no tiene las fichas necesarias para actualizar.~n", [J]),
        fail
    ).

% remove_letras(+FichasJugador, +LetrasUsadas, -Restantes): tiene éxito si las LetrasUsadas están entre las FichasJugador. Predicado auxiliar que elimina las letras de la lista LetrasUsadas de las FichasJugador.
remove_letras(F, [], F).
remove_letras(F, [L|Ls], R) :-
    select(L, F, F1),
    remove_letras(F1, Ls, R).

% reponer_fichas(+Jugador): tiene éxito si el Jugador existe. Repone aleatoriamente las fichas que le faltan al Jugador hasta llegar a 7. Si no hay suficientes fichas en la bolsa, se reponen las que haya disponibles.
reponer_fichas(J) :-
    jugador(J, P, FichasAct),
    length(FichasAct, N),
    M is 7 - N, % Calcula cuántas fichas faltan para completar 7
    total_fichas_disponibles(Bolsa),
    CantidadAReponer is min(M, Bolsa), % Calcula cuántas fichas se pueden reponer
    generar_fichas(CantidadAReponer, Nuevas),
    append(FichasAct, Nuevas, Final),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Final)).

% total_fichas_disponibles(-N): tiene éxito si N es el número total de fichas disponibles en la bolsa. 
total_fichas_disponibles(N) :-
    findall(Cantidad, ficha_disponible(_, Cantidad), Cantidades),
    sum_list(Cantidades, N).

% generar_fichas(+N, -Fichas): tiene éxito si quedan fichas en la bolsa. Genera N fichas aleatorias de la bolsa y las devuelve en la lista Fichas. Si N es 0, devuelve una lista vacía.
generar_fichas(0, []) :- !.
generar_fichas(N, [L|R]) :-
    findall(F, ficha_disponible(F, _), Bolsa),
    Bolsa \= [],
    random_member(L, Bolsa),
    reducir_ficha(L),
    N1 is N - 1,
    generar_fichas(N1, R).

% sacar_fichas(+Letras): tiene éxito si todas las letras de la lista Letras están disponibles en la bolsa. Saca una ficha correspondiente a cada letra (elimina las fichas de la bolsa, reduciendo su cantidad en 1 cada vez).
sacar_fichas(Letras) :-
    todas_disponibles(Letras),      % Verifica sin modificar
    maplist(reducir_ficha, Letras). % Solo modifica si todo está bien

% todas_disponibles(+Letras): tiene éxito si todas las Letras están disponibles en la bolsa.
todas_disponibles([]).
todas_disponibles([L|Ls]) :-
    ficha_disponible(L, Cant),
    Cant > 0,
    todas_disponibles(Ls).

%reducir_ficha(+Letra): tiene éxito si la ficha de la letra Letra está disponible en la bolsa. Reduce la cantidad de la ficha en 1. Si la cantidad llega a 0, elimina la ficha de la bolsa.
reducir_ficha(L) :-
    ficha_disponible(L, Cantidad),
    Cantidad > 0,
    CantidadNueva is Cantidad - 1,
    retract(ficha_disponible(L, Cantidad)),
    assert(ficha_disponible(L, CantidadNueva)).

reducir_ficha(L) :-
    ficha_disponible(L, 0),
    retract(ficha_disponible(L, 0)).

% asignar_fichas(+Jugador, +NuevasFichas): tiene éxito si el Jugador tiene el turno y la partida acaba de empezar o si el Jugador ha formado la última palabra, si el jugador necesita exactamente la cantidad de NuevasFichas para llegar a 7 y si hay suficientes fichas de NuevasFichas en la bolsa. Asigna las NuevasFichas al Jugador.
asignar_fichas(Jugador, NuevasFichas) :-
    (   \+ partida_activa(_)
    ->  format("Error: No hay una partida activa.~n"), fail
    ;   (   findall(jugada(_,_,_,_,_), jugada(_,_,_,_,_), Jugadas),
            (   Jugadas == []
            ->  (   turno_actual(Jugador)
                ->  jugador(Jugador, _, FichasActuales),
                    length(FichasActuales, CantidadActual),
                    length(NuevasFichas, CantidadNueva),
                    (7 is CantidadActual + CantidadNueva
                        -> true
                        ; N is 7 - CantidadActual,
                        format("El jugador necesita ~w fichas para completar 7.~n", [N]), fail
                    ),
                    (   sacar_fichas(NuevasFichas)
                        ->  true
                        ;   format("Error: No hay suficientes fichas en la bolsa.~n"), fail
                    ),
                    append(FichasActuales, NuevasFichas, FichasFinales),
                    retract(jugador(Jugador, Puntuacion, _)),
                    assert(jugador(Jugador, Puntuacion, FichasFinales)),
                    format("Fichas asignadas al jugador ~w: ~w~n", [Jugador, NuevasFichas])
                ;   format("Error: Solo el jugador que tiene el turno puede recibir fichas al inicio.~n"), fail
                )
            ;   last(Jugadas, jugada(Jugador, _,_,_,_))
            ->  jugador(Jugador, _, FichasActuales),
                jugador(Jugador, _, FichasActuales),
                    length(FichasActuales, CantidadActual),
                    length(NuevasFichas, CantidadNueva),
                    (7 is CantidadActual + CantidadNueva
                        -> true
                        ; N is 7 - CantidadActual,
                        format("El jugador necesita ~w fichas para completar 7.~n", [N]), fail
                    ),
                    (   sacar_fichas(NuevasFichas)
                        ->  true
                        ;   format("Error: No hay suficientes fichas en la bolsa.~n"), fail
                    ),
                    append(FichasActuales, NuevasFichas, FichasFinales),
                    retract(jugador(Jugador, Puntuacion, _)),
                    assert(jugador(Jugador, Puntuacion, FichasFinales)),
                    format("Fichas asignadas al jugador ~w: ~w~n", [Jugador, NuevasFichas])
            ;   format("Error: El jugador ~w no ha realizado la última jugada.~n", [Jugador]), fail
            )
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de gestión de información e historial
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% mostrar_puntuacion: tiene éxito si hay una partida activa, muestra la puntuacion actual de los jugadores.
mostrar_puntuacion :-
    partida_activa(jugadores(J1, J2)), !,
    jugador(J1, Puntos1, _),
    jugador(J2, Puntos2, _),
    format("Puntuacion actual:~n"),
    format(" - ~w: ~d puntos~n", [J1, Puntos1]),
    format(" - ~w: ~d puntos~n", [J2, Puntos2]).
mostrar_puntuacion :-
    write('Error: no hay una partida iniciada.'), nl, fail.

% ver_resumen: tiene éxito si hay una partida iniciada, muestra un resumen de la partida actual.
ver_resumen :-
    partida_activa(jugadores(J1, J2)), !,
    format("=== RESUMEN DE LA PARTIDA ===~n~n"),

    % Configuración
    format("Configuracion:~n"),
    ( opcion(idioma, Idioma) -> format(" - Idioma: ~w~n", [Idioma]) ; true ),
    ( opcion(modo, Modo) -> format(" - Modo de juego: ~w~n", [Modo]) ; true ),
    format(" - Jugadores: ~w y ~w~n~n", [J1, J2]),

    % Historial
    format("Historial de jugadas:~n"),
    forall(jugada(J, P, Pts,_, Fichas),
           (format(" - ~w ha colocado '~w' (+~d pts), fichas restantes: ~w~n", [J, P, Pts, Fichas]))).
ver_resumen :-
    write('Error: no hay una partida iniciada.'), nl, fail.


% ver_historial(+Jugador): tiene éxito si el Jugador tiene un historial guardado. Muestra el historial de Jugador con sus victorias, derrotas, puntuacion máxima y media.
ver_historial(J) :-
    ruta_historial(Ruta),
    open(Ruta, read, Stream),
    leer_historial(Stream, J, 0, 0, 0, 0, V, D, Max, Media),
    close(Stream),
    format(" ====Historial==== de ~w:~n", [J]),
    format(" - Victorias: ~d~n", [V]),
    format(" - Derrotas: ~d~n", [D]),
    format(" - Puntuacion máxima: ~d~n", [Max]),
    format(" - Puntuacion media: ~2f~n", [Media]).

% actualizar_historial(+Jugador, +Resultado, +Puntos): siempre tiene éxito, actualiza el historial del jugador Jugador con el resultado Resultado y los puntos Puntos.
actualizar_historial(J, Resultado, Puntos) :-
    ruta_historial(Ruta),
    open(Ruta, append, Stream),
    format(Stream, "~w,~w,~d~n", [J, Resultado, Puntos]),
    close(Stream).

% ruta_historial(-Ruta): siempre tiene éxito, devuelve la ruta del archivo de historial.
ruta_historial(RutaCompleta) :-
    source_file(ruta_historial(_), RutaFuente),
    file_directory_name(RutaFuente, Dir),
    atomic_list_concat([Dir, '/historial.csv'], RutaCompleta).

% leer_historial(+Stream, +Jugador, +Victorias, +Derrotas, +Suma, +N, -VictoriasFinales, -DerrotasFinales, -MaxPuntos, -Media): predicado auxiliar, tiene éxito si el Jugador tiene un historial guardado. Lee el historial de un jugador desde el archivo y calcula sus estadísticas VictoriasFinales, DerrotasFinales, MaxPuntos y Media.
leer_historial(Stream, _, V, D, Sum, N, V, D, Max, Media) :-
    at_end_of_stream(Stream), !,
    (N =:= 0 -> Media = 0 ; Media is Sum / N),
    (N =:= 0 -> Max = 0 ; Max = Sum).  

leer_historial(Stream, J, V0, D0, S0, N0, V, D, Max, Media) :-
    read_line_to_string(Stream, Line),
    split_string(Line, ",", "", [Jugador, Resultado, PuntosStr]),
    atom_string(JStr, Jugador),
    JStr = J,
    number_string(Puntos, PuntosStr),
    (Resultado = "victoria" -> V1 is V0 + 1, D1 is D0 ; V1 = V0, D1 is D0 + 1),
    S1 is S0 + Puntos,
    N1 is N0 + 1,
    leer_historial(Stream, J, V1, D1, S1, N1, V, D, Max, Media).

leer_historial(Stream, J, V0, D0, S0, N0, V, D, Max, Media) :-
    leer_historial(Stream, J, V0, D0, S0, N0, V, D, Max, Media).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de finalización de partida
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% finalizar_partida: tiene éxito si ya no hay más fichas que repartir o si ambos jugadores han pasado el turno. Finaliza la partida actual, muestra el ganador y actualiza el historial de los jugadores.
finalizar_partida :-
    (   bolsa_vacia
    ;   se_pasan_turno_ambos
    ),
    partida_activa(jugadores(J1, J2)),
    jugador(J1, P1, _),
    jugador(J2, P2, _),
    (   P1 > P2
    ->  Ganador = J1, Perdedor = J2, MaxP = P1, MinP = P2
    ;   P2 > P1
    ->  Ganador = J2, Perdedor = J1, MaxP = P2, MinP = P1
    ;   write('La partida ha terminado en empate.'), nl, !, fail
    ),
    format("Partida finalizada. Ganador: ~w con ~d puntos.~n", [Ganador, MaxP]),
    actualizar_historial(Ganador, victoria, MaxP),
    actualizar_historial(Perdedor, derrota, MinP),
    reset_juego.

% bolsa_vacia: tiene éxito si la bolsa de fichas está vacía.
bolsa_vacia :-
    \+ (ficha_disponible(_, C), C > 0).

% se_pasan_turno_ambos: tiene éxito si ambos jugadores pasan el turno seguidamente.
se_pasan_turno_ambos :-
    turnos_sin_jugar(2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Predicados de cálculo de estadísticas y ranking
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ver_ranking: siempre tiene éxito, muestra dos rankings. 1. Por porcentaje de victorias, 2. Por puntuación media.
ver_ranking :-
    ruta_historial(Ruta),
    open(Ruta, read, Stream),
    leer_todas_jugadas(Stream, [], DatosPorJugador),
    close(Stream),
    mostrar_ranking_victorias(DatosPorJugador),
    nl,
    mostrar_ranking_puntuaciones(DatosPorJugador).

% leer_todas_jugadas(+Stream, +Acum, -Datos): siempre tiene éxito, lee todas las jugadas del archivo y las almacena en la lista Acum. Datos es una lista de estadísticas de cada jugador
leer_todas_jugadas(Stream, Acum, DatosFinal) :-
    at_end_of_stream(Stream), !,
    DatosFinal = Acum.
leer_todas_jugadas(Stream, Acum, DatosFinal) :-
    read_line_to_string(Stream, Line),
    split_string(Line, ",", "", [JugadorStr, Resultado, PuntosStr]),
    atom_string(J, JugadorStr),
    number_string(Puntos, PuntosStr),
    actualizar_stats(J, Resultado, Puntos, Acum, Actualizado),
    leer_todas_jugadas(Stream, Actualizado, DatosFinal).

% actualizar_stats(+Jugador, +Resultado, +Puntos, +Acum, -NuevoAcum): siempre tiene éxito, actualiza las estadísticas del jugador Jugador con el resultado Resultado y los puntos Puntos. Acum es la lista acumulada de estadísticas.
actualizar_stats(J, Resultado, Pts, [], [stats(J, V, D, Pts, 1)]) :-
    (Resultado = "victoria" -> V = 1, D = 0 ; V = 0, D = 1).
actualizar_stats(J, Resultado, Pts, [stats(J, V0, D0, S0, N0)|Resto], [stats(J, V, D, S, N)|Resto]) :-
    !,
    (Resultado = "victoria" -> V is V0 + 1, D = D0 ; V = V0, D is D0 + 1),
    S is S0 + Pts,
    N is N0 + 1.
actualizar_stats(J, Resultado, Pts, [X|R], [X|R2]) :-
    actualizar_stats(J, Resultado, Pts, R, R2).

% mostrar_ranking_victoria: siempre tiene éxito, muestra el ranking de jugadores por porcentaje de victorias. Datos es una lista de estadísticas de cada jugador.
mostrar_ranking_victorias(Datos) :-
    maplist(calcular_porcentaje_victorias, Datos, ListaConPorc),
    sort(2, @>=, ListaConPorc, Ordenado),
    write(' Ranking por porcentaje de victorias:'), nl,
    forall(member(J-Victorias-Porc, Ordenado),
        format(" - ~w: ~d victorias (~2f%)~n", [J, Victorias, Porc])).

% calcular_porcentaje_victorias(+Stats, -J-V-Porcentaje): siempre tiene éxito, calcula el porcentaje de victorias J-V-Porcentaje de un jugador dadas sus Stats.
calcular_porcentaje_victorias(stats(J, V, D, _, _), J-V-Porcentaje) :-
    Total is V + D,
    (Total =:= 0 -> Porcentaje = 0 ; Porcentaje is (V * 100) / Total).

% mostrar_ranking_puntuaciones(+Datos): siempre tiene éxito, muestra el ranking de jugadores por puntuación media.
mostrar_ranking_puntuaciones(Datos) :-
    maplist(calcular_puntuaciones, Datos, ListaConPuntos),
    sort(3, @>=, ListaConPuntos, Ordenado),
    write('Ranking por puntuación media:'), nl,
    forall(member(puntos(J, Max, Media), Ordenado),
        format(" - ~w: máx. ~d, media ~2f~n", [J, Max, Media])).

% calcular_puntuaciones(+Stats, -Puntos): siempre tiene éxito, calcula la puntuación máxima y media de un jugador.
calcular_puntuaciones(stats(J, _, _, Suma, N), puntos(J, Max, Media)) :-
    Media is Suma / N,
    Max is Suma.