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

% valores por defecto
:- dynamic opcion_inicializada/0.
:- initialization(init_config).

init_config :-
    opcion_inicializada, !.
init_config :-
    assert(opcion(idioma, es)),
    assert(opcion(modo_juego, jugadorVSjugador)),
    assert(opcion(reparto_fichas, aleatorio)),
    assert(opcion(modo_inicio, normal)),
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
ruta_diccionario(es, 'palabras_castellano.txt').
ruta_diccionario(eus,    'palabras_euskera.txt').
ruta_diccionario(en,     'palabras_ingles.txt').

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
    format("Error: No se ha configurado el modo de juego correctamente. Ejecute el predicado configurar_opciones ~n"), fail.

% iniciar_partida/1: Inicia una partida con un jugador contra la máquina
iniciar_partida(Jugador) :-
    nonvar(Jugador),
    opcion(modo_juego, 'jugadorVSmaquina'), % Verifica que el modo de juego sea correcto
    \+ partida_activa(_),
    assert(partida_activa(jugadores(Jugador, maquina))),
    inicializar_jugadores([Jugador, maquina]),
    format("Cargando...\n"),
    inicializar_tablero,
    cargar_diccionario,
    inicializar_bolsa,
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
    format("Cargando...\n"),
    inicializar_tablero,
    cargar_diccionario,
    inicializar_bolsa,
    repartir_fichas([J1, J2]),
    inicializar_turno([J1, J2]),
    turno_actual(T),
    format("Partida iniciada: ~w vs ~w.~n", [J1, J2]),
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
    assert(turno_actual(J1)).

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
    jugador(J, P, _),
    generar_fichas(7, Fichas),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Fichas)),
    repartir_fichas(R).

% mostrar_fichas(+Jugador)
mostrar_fichas(J) :-
    jugador(J, _, Fichas),
    format("Fichas de ~w: ~w~n", [J, Fichas]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% GESTIÓN DE FICHAS POR IDIOMA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% inicializar_bolsa/0 - Prepara la bolsa de fichas según el idioma actual
inicializar_bolsa :-
    retractall(ficha_disponible(_, _)),
    opcion(idioma, Idioma),
    fichas_por_idioma(Idioma, Distribucion),
    agregar_fichas(Distribucion).

% fichas_por_idioma(+Idioma, -Distribucion)
% Distribución = [letra-cantidad, ...]
fichas_por_idioma(es, [a-12, e-12, o-9, s-6, r-5, n-5, l-4, d-3, t-4, u-5, i-6, c-4, m-2, p-2, b-2, g-2, v-1, h-2, f-1, y-1, j-1, 'ñ'-1, q-1, z-1, x-1]).
fichas_por_idioma(eus, [a-14, e-12, i-9, o-6, u-6, n-8, d-4, t-8, l-2, r-7, k-5, g-2, b-2, z-2, m-1, s-2, h-2, p-1, x-1, j-1, f-1]).
fichas_por_idioma(en, [e-12, a-9, i-9, o-8, n-6, r-6, t-6, l-4, s-4, u-4, d-4, g-3, b-2, c-2, m-2, p-2, f-2, h-2, v-2, w-2, y-2, k-1, j-1, x-1, q-1, z-1]).

% agregar_fichas(+ListaLetraCantidad)
agregar_fichas([]).
agregar_fichas([Letra-Cant | Resto]) :-
    forall(between(1, Cant, _), assert(ficha_disponible(Letra, 1))),
    agregar_fichas(Resto).

% ver_bolsa/0 - Muestra cuántas fichas quedan en la bolsa, agrupadas por letra
ver_bolsa :-
    findall(L, ficha_disponible(L, _), Todas),
    msort(Todas, Ordenadas),
    agrupar_fichas(Ordenadas, Agrupadas),
    imprimir_bolsa(Agrupadas).

% agrupar_fichas(+ListaLetras, -ListaLetra-Cantidad)
agrupar_fichas([], []).
agrupar_fichas([H|T], [H-N|R]) :-
    same_letter_count(H, [H|T], N, Resto),
    agrupar_fichas(Resto, R).

same_letter_count(_, [], 0, []).
same_letter_count(X, [X|T], N, Resto) :-
    same_letter_count(X, T, N1, Resto),
    N is N1 + 1.
same_letter_count(X, [Y|T], 0, [Y|T]) :-
    X \= Y.

% imprimir_bolsa(+ListaLetra-Cantidad)
imprimir_bolsa([]).
imprimir_bolsa([L-C|R]) :-
    format("~w: ~d~n", [L, C]),
    imprimir_bolsa(R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TABLERO
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Casilla_Especial
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

% inicializar_tablero/0
inicializar_tablero :-
    retractall(casilla(_,_,_,_)),
    forall(between(1,15,F),
        forall(between(1,15,C),
            assert(casilla(F, C, libre, none)))).

% ver_tablero/0
ver_tablero :-
    write('    '), % Espacio inicial para los números de columna
    forall(between(1, 15, C), (format("~|~`0t~d~2+", [C]), write('  '))), nl, % Encabezado de columnas
    write('   '), % Espacio inicial para la línea superior
    forall(between(1, 15, _), write('----')), nl, % Línea superior de la cuadrícula
    forall(between(1, 15, F), (
        format("~|~`0t~d~2+ |", [F]), % Número de fila con separación
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
            
            write('|') % Separador vertical
        )),
        nl,
        write('   '), % Espacio inicial para las líneas horizontales
        forall(between(1, 15, _), write('----')), nl % Línea horizontal entre filas
    )).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% VALOR PALABRA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% valor_letra(+Letra, +Idioma, -Puntos)
valor_letra(a, es, 1) :- !.
valor_letra(e, es, 1) :- !.
valor_letra(o, es, 1) :- !.
valor_letra(s, es, 1) :- !.
valor_letra(r, es, 1) :- !.
valor_letra(n, es, 1) :- !.
valor_letra(l, es, 1) :- !.
valor_letra(d, es, 2) :- !.
valor_letra(t, es, 1) :- !.
valor_letra(u, es, 1) :- !.
valor_letra(i, es, 1) :- !.
valor_letra(c, es, 3) :- !.
valor_letra(m, es, 3) :- !.
valor_letra(p, es, 3) :- !.
valor_letra(b, es, 3) :- !.
valor_letra(g, es, 2) :- !.
valor_letra(v, es, 4) :- !.
valor_letra(h, es, 4) :- !.
valor_letra(f, es, 4) :- !.
valor_letra(y, es, 4) :- !.
valor_letra(j, es, 8) :- !.
valor_letra('ñ', es, 8) :- !.
valor_letra(q, es, 5) :- !.
valor_letra(z, es, 10) :- !.
valor_letra(x, es, 8) :- !.
valor_letra(w, es, 4) :- !.
valor_letra(k, es, 5) :- !.

valor_letra(a, eus, 1).
valor_letra(e, eus, 1).
valor_letra(i, eus, 1).
valor_letra(o, eus, 1).
valor_letra(u, eus, 1).
valor_letra(n, eus, 1).
valor_letra(d, eus, 2).
valor_letra(t, eus, 1).
valor_letra(l, eus, 1).
valor_letra(r, eus, 1).
valor_letra(k, eus, 2).
valor_letra(g, eus, 2).
valor_letra(b, eus, 2).
valor_letra(z, eus, 4).
valor_letra(m, eus, 2).
valor_letra(s, eus, 2).
valor_letra(h, eus, 4).
valor_letra(p, eus, 3).
valor_letra(x, eus, 4).
valor_letra(j, eus, 4).
valor_letra(v, eus, 3).
valor_letra(f, eus, 4).
valor_letra(w, eus, 4).
valor_letra(c, eus, 5).
valor_letra(q, eus, 5).
valor_letra(y, eus, 5).
valor_letra(_, eus, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FORMAR PALABRA
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% puntuaje_palabra(+Letras, +Idioma, +Orientacion, +Fila, +Columna, -Puntos)
puntuaje_palabra(Letras, Idioma, O, F, C, PuntosTotales) :-
    puntuar_letras(Letras, Idioma, O, F, C, 0, 1, PuntosTotales).

% puntuar_letras(+Letras, +Idioma, +Orientacion, +F, +C, +Acum, +MultPalabra, -PuntosFinal)
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

% letra_valor(+Letra, +Fila, +Columna, +Idioma, -ValorFinal, -MultPalabra)
letra_valor(L, F, C, Idioma, ValorLetra, PMult) :-
    downcase_atom(L, Lmin),
    valor_letra(Lmin, Idioma, Base),
    (   casilla(F, C, libre, _) ->
        (   casilla_especial(F, C, letra, LMult) -> ValorLetra is Base * LMult ; ValorLetra = Base ),
        (   casilla_especial(F, C, palabra, PMult) -> true ; PMult = 1 )
    ;   ValorLetra = Base,
        PMult = 1
    ).

% filepath: c:\Users\uleon\Desktop\TRAGICERA6\PL\practicaPL\scrabble.pl
% formar_palabra(+Jugador, +Orientacion, +Fila, +Columna, +Palabra)
formar_palabra(J, O, F, C, P) :-
    format("Intentando formar la palabra '~w' por el jugador ~w en orientación ~w desde la posición (~w, ~w).~n", [P, J, O, F, C]),
    (   partida_activa(_)
    ->  format("Partida activa verificada.~n")
    ;   format("Error: No hay una partida activa.~n"), fail
    ),
    (   turno_actual(J)
    ->  format("Es el turno del jugador ~w.~n", [J])
    ;   format("Error: No es el turno del jugador ~w.~n", [J]), fail
    ),
    (   jugador(J, _, Fichas)
    ->  format("Fichas del jugador ~w: ~w.~n", [J, Fichas])
    ;   format("Error: El jugador ~w no está registrado.~n", [J]), fail
    ),
    string_upper(P, Mayus),
    (   palabra_valida(Mayus)
    ->  format("La palabra '~w' es válida en el diccionario.~n", [P])
    ;   format("Error: La palabra '~w' no está en el diccionario.~n", [P]), fail
    ),
    atom_chars(P, Letras),
    (   puede_colocar(P, O, F, C)
    ->  format("La palabra '~w' puede colocarse en el tablero.~n", [P])
    ;   format("Error: La palabra '~w' no puede colocarse en el tablero.~n", [P]), fail
    ),
    letras_en_tablero(Letras, O, F, C, LetrasEnTablero),
    format("Letras ya colocadas en el tablero: ~w.~n", [LetrasEnTablero]),
    subtract(Letras, LetrasEnTablero, LetrasUsadas), % Calcula las letras que el jugador realmente usó
    format("Letras realmente usadas por el jugador: ~w.~n", [LetrasUsadas]),
    append(Fichas, LetrasEnTablero, FichasCombinadas),
    format("Fichas combinadas (jugador + tablero): ~w.~n", [FichasCombinadas]),
    (   contiene_fichas(FichasCombinadas, Letras)
    ->  format("El jugador ~w tiene las fichas necesarias para formar la palabra '~w'.~n", [J, P])
    ;   format("Error: El jugador ~w no tiene las fichas necesarias para formar la palabra '~w'.~n", [J, P]), fail
    ),
    sumar_puntos(J, Letras, O,F,C),
    colocar_palabra(Letras, O, F, C),
    format("La palabra '~w' ha sido colocada en el tablero.~n", [P]),
    actualizar_fichas_jugador(J, LetrasUsadas), % Solo elimina las letras realmente usadas
    format("Las fichas del jugador ~w han sido actualizadas.~n", [J]),
    reponer_fichas(J),
  % format("Las fichas del jugador ~w han sido repuestas.~n", [J]),
    
    format("Los puntos del jugador ~w han sido actualizados.~n", [J]).

formar_palabra(J, _, _, _, _) :-
    \+ turno_actual(J),
    format("Error: No es el turno del jugador ~w.~n", [J]),
    fail.

% letras_en_tablero(+Letras, +Orientacion, +Fila, +Columna, -LetrasEnTablero)
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

/*
% VERSION ORIGINAL puede_colocar(+Palabra, +Orientacion, +Fila, +Columna)
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
    )).*/

% puede_colocar(+Palabra, +Orientacion, +Fila, +Columna)
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

% colocar_letra(+Fila, +Columna, +Letra)
colocar_letra(F, C, L) :-
    (   casilla(F, C, libre, none) % Si la casilla está libre
    ->  retract(casilla(F, C, libre, none)), % Elimina la casilla libre
        assert(casilla(F, C, ocupada, L)) % Coloca la letra en la casilla
    ;   casilla(F, C, ocupada, L) % Si la casilla ya está ocupada con la misma letra
    ->  true % No hace nada, ya está colocada correctamente
    ;   format("Error: No se puede colocar la letra ~w en la casilla (~w, ~w).~n", [L, F, C]),
        fail % Falla si la casilla está ocupada con una letra diferente
    ).

% contiene_fichas(+FichasJugador, +LetrasPalabra)
contiene_fichas(FJ, Ls) :-
    msort(FJ, S1), msort(Ls, S2), sublista(S2, S1).

% sublista(S, L): S está contenida en L
sublista([], _).
sublista([X|Xs], [X|Ys]) :- sublista(Xs, Ys).
sublista(Xs, [_|Ys]) :- sublista(Xs, Ys).

% actualizar_fichas_jugador(+Jugador, +LetrasUsadas)
actualizar_fichas_jugador(J, Letras) :-
    jugador(J, P, F),
    format("Fichas actuales del jugador ~w: ~w.~n", [J, F]),
    format("Letras a eliminar: ~w.~n", [Letras]),
    (   contiene_fichas(F, Letras) % Verifica que el jugador tiene las fichas necesarias
    ->  remove_letras(F, Letras, FR),
        format("Fichas restantes después de eliminar: ~w.~n", [FR]),
        retract(jugador(J, P, _)),
        assert(jugador(J, P, FR))
    ;   format("Error: El jugador ~w no tiene las fichas necesarias para actualizar.~n", [J]),
        fail
    ).

% remove_letras(+FichasJugador, +LetrasUsadas, -Restantes)
remove_letras(F, [], F).
remove_letras(F, [L|Ls], R) :-
    select(L, F, F1),
    remove_letras(F1, Ls, R).


% sumar_puntos(+Jugador, +Letras, +O, +F, +C)
sumar_puntos(J, Letras, O, F, C) :-
    opcion(idioma, Idioma),
    puntuaje_palabra(Letras, Idioma, O, F, C, Puntos),
    jugador(J, Anterior, Fichas),
    retract(jugador(J, Anterior, Fichas)),
    Nuevo is Anterior + Puntos,
    assert(jugador(J, Nuevo, Fichas)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FUNCIONES AUXILIARES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% reponer_fichas(+Jugador)
reponer_fichas(J) :-
    jugador(J, P, FichasAct),
    length(FichasAct, N),
    M is 7 - N, % Calcula cuántas fichas faltan para completar 7
    format("El jugador ~w tiene ~w fichas. Necesita reponer ~w fichas.~n", [J, N, M]),
    total_fichas_disponibles(Bolsa),
    format("Fichas disponibles en la bolsa: ~w.~n", [Bolsa]),
    min(M, Bolsa, CantidadAReponer), % Calcula cuántas fichas se pueden reponer
    generar_fichas(CantidadAReponer, Nuevas),
    %format("Fichas nuevas para el jugador ~w: ~w.~n", [J, Nuevas]),
    append(FichasAct, Nuevas, Final),
    retract(jugador(J, P, _)),
    assert(jugador(J, P, Final)).
    % format("Fichas finales del jugador ~w: ~w.~n", [J, Final]).

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
