

% playAskColor
% Ask the color for the human player and start the game with it.
%

%Crea un arbol de jugadas posibles
%parametros: mis_fichas, fichas_ocultas, tablero 
%Nuestra ficha a jugar va a ser el ultimo max

%Nota toma en cuenta caso base donde ya no tienes fichas que poner

%vemos que fichas se pueden jugar
jugadas(Fichas_posibles, [X,Y], JugadasX, JugadasY):-
    jugadas(Fichas_posibles, X, JugadasX),
    jugadas(Fichas_posibles, Y, JugadasY).


jugadas([[A,B]|Fichas_posibles], Z, Jugadas):-
    jugadas(Fichas_posibles, Z, JugadasParcial),
    ((A =:= Z; B =:= Z) ->
        Jugadas = [[A,B]|JugadasParcial];
        Jugadas = JugadasParcial).

jugadas([],_,[]).


%crea el arbol de jugadas posibles

%Elegi que la pieza -10000 sea indicar que es una hoja
arbol_jugadas([],_,_,_,_,-1000,_):-
    ! . %hasta que se acaben mis fichas
arbol_jugadas(_,[],_,_,_,-1000,_):-
    ! .%hasta que se acaben las fichas ocultas
arbol_jugadas(_,_,_,5,_,-1000,_):-
    ! . %hasta que se acabe el nivel

arbol_jugadas(Mis_fichas, Fichas_ocultas, [X,Y], Nivel, Ficha_a_jugar, Val, BanderaFin):-

    %para cada ficha en JugadasX y en Jugadas Y elimino ficha y calculo jugadas
    %no olvidar calcular el modulo para saber si calculamos mis jugadas o jugadas del oponente

    ((Nivel mod 2) =:= 0 ->
        jugadas(Mis_fichas, [X,Y], JugadasX, JugadasY)
        ;
        jugadas(Fichas_ocultas, [X,Y], JugadasX, JugadasY)),



    %llamar a recorre

    recorre_fichas_arbol(Mis_fichas, Fichas_ocultas, JugadasX, [X,Y], x , Nivel, Ficha_a_jugar1, Val1),
    recorre_fichas_arbol(Mis_fichas, Fichas_ocultas, JugadasY, [X,Y], c , Nivel, Ficha_a_jugar2, Val2),

    %elijo la mejor jugada
    ((Nivel mod 2) =:= 0 ->
        %maximizar
        (Val1 > Val2 ->
            Val = Val1,
            Ficha_a_jugar = Ficha_a_jugar1,
            BanderaFin = x;
            Val = Val2,
            Ficha_a_jugar = Ficha_a_jugar2,
            BanderaFin = c)
        ;
        %minimizar
        (Val1 < Val2 ->
            Val = Val1,
            Ficha_a_jugar = Ficha_a_jugar1,
            BanderaFin = x;
            Val = Val2,
            Ficha_a_jugar = Ficha_a_jugar2,
            BanderaFin = c)),


    (Nivel =:= 0 ->
        !;
        true).


recorre_fichas_arbol(_, _, [],_,_,Nivel,[],Val):- %ultima jugada del arreglo
    ((Nivel mod 2) =:= 0 ->
        Val = -1000
        ;
        Val = 1000).

%suponemos que el recorre fichas ya me regresa la mejor ficha posible
%Define donde regresar la ficha dentro de recorre
recorre_fichas_arbol(FichasActualmente, FichasOcultas, [[A,B]|Jugadas], [X,Y], Bandera, Nivel ,FichaC, Val):-
    %recorre manda a llamar a eliminar
    %El nivel para recorre nunca cambia 
    %recorre JX y JY manda siempre FichasActualmente
    %por cada ficha en Jugadas la elimina 
    %y ya con cada elimina
    %Hacemos modulo dos para si eliminamos de FichasActualmente o FichaOcultas

    ((Nivel mod 2) =:= 0 ->
        %entonces es max (creo)
        %elimino ficha de FichasActualmente
        eliminar(FichasActualmente, FichasOcultas, [A,B], [X,Y], Bandera, Nivel, _, Val1),

        %recorre al sig elemento de Jugadas
        recorre_fichas_arbol(FichasActualmente, FichasOcultas, Jugadas, [X,Y], Bandera, Nivel, Ficha, Val2),
        %calcula el maximo de Val1 y Val2
        max(Val1, Val2, [A,B], Ficha, FichaC, Val)
        ;
        %entonces es min (creo)
        eliminar(FichasActualmente, FichasOcultas, [A,B], [X,Y], Bandera, Nivel, _, Val1),

        %recorre al sig elemento de Jugadas
        recorre_fichas_arbol(FichasActualmente, FichasOcultas, Jugadas,[X,Y],Bandera, Nivel, Ficha, Val2),
        %calcula el minimo de Val1 y Val2
        min(Val1, Val2, [A,B], Ficha ,FichaC, Val)
        )
    .


eliminar(FichasActualmente, FichasOcultas, [A,B], [X,Y], Bandera,  Nivel , Ficha, ValorMinMax):-
    %elimina la ficha de mis fichas
    %cambia el tablero
    %elimina llama a arbol_jugadas


    modifica_tablero([X,Y], [A,B], Bandera, TableroNuevo),

    %checamos modulo 2 para ver si viene de nivel par o impar
    ((Nivel mod 2) =:= 0 ->

        restaF(FichasActualmente, [[A,B]], FichasResultantes),

        NivelNuevo is Nivel + 1,
        arbol_jugadas(FichasResultantes, FichasOcultas, TableroNuevo, NivelNuevo,FichaR, Valor,_) %Chance se tienen que modificar cosas aqui
        ;

        restaF(FichasOcultas, [[A,B]], FichasResultantes),

        NivelNuevo is Nivel + 1,
        arbol_jugadas(FichasActualmente, FichasResultantes, TableroNuevo, NivelNuevo, FichaR, Valor,_)
        ),

    (Valor =:= -1000 ->
    %Es hoja entonces sacamos Heuristica,
    heuristica_jugada(FichasActualmente, FichasOcultas, Nivel, ValorMinMax),
    Ficha = [A,B]
    ;
    ValorMinMax is Valor,
    Ficha = FichaR
    ).




max(Val1, Val2, Fich1, Fich2, Fich, Val):-
    (Val1 > Val2 ->
        Fich = Fich1,
        Val = Val1;
        Fich = Fich2,
        Val = Val2).

min(Val1, Val2, Fich1, Fich2, Fich, Val):-
    (Val1 < Val2 ->
        Fich = Fich1,
        Val = Val1;
        Fich = Fich2,
        Val = Val2).


%Modifica tablero
modifica_tablero([X,Y], [A,B], Bandera, [ColaX, ColaY]):-
    %checamos si es x o y
    (Bandera == x ->
        %es x
        ColaY is Y,
        (X =:= A ->
            ColaX is B 
            ;
            ColaX is A)        
        ;
        %es y
        ColaX is X,
        (Y =:= A ->
            ColaY is B 
            ;
            ColaY is A)
    ).

%saca la heuristica de una jugada de domino
heuristica_jugada(FichasActualmente, FichasOcultas,Nivel, Val):-

    ((Nivel mod 2) =:= 0 ->
        %sacamos heuristica para mis fichas
        (FichasActualmente == [] ->
            Val is 100
            ;
            (FichasOcultas == [] ->
                Val is -100
                ;

                ValInt is 50,
                (tiene_ficha_doble(FichasActualmente) ->
                    ValAux is ValInt - 20
                    ;
                    ValAux is ValInt +20
                    ),
                numeros_posibles(FichasActualmente,[],Posibles),
                Val is ValAux + (Posibles*2)

            )
        )
        ;
        %sacamos heuristica para las fichas del oponente
        (FichasOcultas == [] ->
            Val is 100
            ;
            (FichasActualmente == [] ->
                Val is -100
                ;
                ValInt is 50,
                (tiene_ficha_doble(FichasOcultas) ->
                    ValAux is ValInt - 20
                    ;
                    ValAux is ValInt +20
                    ),
                numeros_posibles(FichasOcultas,[],Posibles),
                Val is ValAux + (Posibles*2)

            )
        )
        ).

%Funcion que cuenta cuantos elementos hay en una lista
longitud([], 0).
longitud([_|L], N) :-
    longitud(L, N1),
    N is N1 + 1.


%Funcion que encuentra si tiene una ficha doble
tiene_ficha_doble(Fichas):-
    member([X,X], Fichas),
    !.

%Funcion que te dice cuales nums tiene
numeros_posibles([[A,B]|Fichas], Aux , Lista):-
        (not(memberchk(A, Aux)) ->
                append(Aux, [A], Aux1);
                Aux1 = Aux
        ),
        (not(memberchk(B, Aux1)) ->
                append(Aux1, [B], Aux2);
                Aux2 = Aux1
        ),
        numeros_posibles(Fichas,Aux2, Lista).

numeros_posibles([], Aux, Res):-
        length(Aux, Res).
        

inicio:-
        nl, write('Empieza yo? (y or n)'), nl,
        read(Jugador), nl,
        write('mis fichas '), nl, read(Fichas), nl,
        subtract([[0,0], [0,1], [0,2], [0,3], [0,4], [0,5], [0, 6], [1,1], [1,2], [1,3], [1,4], [1,5], [1,6], [2,2], [2,3], [2,4], [2,5], [2,6], [3,3], [3,4], [3,5], [3,6], [4,4], [4,5], [4,6], [5,5], [5,6], [6,6]], Fichas, Oponentfichas), nl,
        write(Oponentfichas),nl,


                        (
                Jugador \= n, !,% si empiezo yo
                write("Con qu� empezamos?"),%Arreglo de 2
                read(Tablero),
                restaF(Oponentfichas, Tablero, FichasRestantes),
                playOther(Fichas, FichasRestantes)

                ;
                write("Con qu� empez� el otro?"),
                read(Tablero),
                play(Fichas,Oponentfichas,Tablero),


                write("segundo")
            ).

play([],_,_):-
        write("Gan� yo"),
        !.
play(_,[],_):-
        write("Gan� el otro"),
        !.

play(Fichas,X,Tablero):-
        write('Comemos? (y,n)'), nl,
        read(Res), nl,
        (   Res\=n,!,
            write("Qu� comimos?"),
            read(Ficha),
            agregaF([Ficha],Fichas,FichasR),
            restaF(X,[Ficha],FichasF), %Ver donde debemos de jugar FichasF
            write("Nuestras fichas: "),
            write(FichasR), nl,
            play(FichasR, FichasF, Tablero); %Puse FichasF en lugar de X aqui !!!!!


        write(Fichas),nl,
        %Falta funci�n heuristica que devuelve Ficha
        write("FICHAS NUESTRAS: "),write(Fichas),nl,
        arbol_jugadas(Fichas, X, Tablero, 0, Ficha_a_jugar, Val, BanderaFin),
        write("Ficha a jugar: "),write(Ficha_a_jugar),nl,
        write("Valor: "),write(Val),nl,
        write('Lado del Tablero: '),write(BanderaFin),nl,
        write('Tablero: '),write(Tablero),nl,
        restaF(Fichas,[Ficha_a_jugar],FichasF),
        playOther(FichasF, X),

                write(X),nl,
        write(Fichas)).

playOther(Y,X):-
        write('Qu� jug� el otro?'),nl,
        read(Ficha),
        write("Como qued� el tablero?"),nl,
        read(Tablero2),

        restaF(X,[Ficha],FichasF),
        write(FichasF),nl,
        play(Y,FichasF,Tablero2).


restaF([], _, []) :- !. %Si est� vac�o devolvemos vac�o
restaF([[X,Y]|C], B, D) :-
        (memberchk([X,Y], B); memberchk([Y,X], B)), !,%Si est� la ficha en el conjunto
        restaF(C, B, D).%la quitamos
restaF([A|B], C, [A|D]) :- %siguiente ficha a sub
        restaF(B, C, D).
agregaF([],F,F):-
        !.
agregaF([X|F1],Fichas,[X|Res]):-
        agregaF(F1,Fichas,Res).

valid_fichas([[X,Y]|Tail], Fichas):-
    (not(memberchk([X,Y], Fichas)), not(memberchk([Y,X], Fichas)), !),
    valid_fichas(Tail, Fichas).

valid_fichas([], _).
