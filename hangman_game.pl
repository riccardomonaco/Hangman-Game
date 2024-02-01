/* Lista di parole segrete*/
paroleSegrete(['haskell', 'programmazione', 'funzionale', 'linguaggio', 'computazione']).


/*Predicato main del gioco che instanzia l 'ambiente di gioco*/
main :-
    preparazioneAmbienteDiGioco(6).

/*Predicato per selezionare casualmente una parola segreta dalla lista di paroleSegrete*/
selezionaParolaSegreta(Parola) :-
    paroleSegrete(ListaParole),
    length(ListaParole, Lunghezza),
    random(0, Lunghezza, Indice),
    nth0(Indice, ListaParole, ParolaAtomizzata),
    atom_chars(ParolaAtomizzata, Parola).
    aaaaaaaaaa

/* Predicato per la preparazione dell'ambiente di gioco, dove viene dato il benvenuto, disegnato il patibolo vuoto e selezionata casualmente la parola da indovinare */
preparazioneAmbienteDiGioco(TentativiRimanenti) :-
    pulisciConsole,
    write('Benvenuto nel gioco dell Impiccato!'), nl,
    selezionaParolaSegreta(ParolaSegreta),
    gioca(ParolaSegreta, [], TentativiRimanenti).

/*Preicato che gestisce la fase di gioco */
gioca(ParolaSegreta, TentativoAttuale, TentativiRimanenti) :-
    (   TentativiRimanenti =:= 0 ->
        pulisciConsole,
        disegnaPatibolo(0),
        write('Hai perso! Il tuo omino è stato impiccato.'), nl,
        write('La parola segreta era: '),write_list(ParolaSegreta), nl
    ;   parolaIndovinata(ParolaSegreta, TentativoAttuale) ->
        pulisciConsole,
        visualizzaParola(ParolaSegreta, TentativoAttuale), nl,
        write('Hai vinto! La parola segreta era: '), stampa_lista(ParolaSegreta), nl
    ;   pulisciConsole,
        write('Parola attuale:'), nl,
        visualizzaParola(ParolaSegreta, TentativoAttuale), nl, nl,
        disegnaPatibolo(TentativiRimanenti),
        write('Tentativi rimanenti: '), write(TentativiRimanenti), nl, nl,
        write('Indovina una lettera: '), nl,
        get_char(Lettera),
        get_char(_),
        aggiornaTentativo(ParolaSegreta, TentativoAttuale, Lettera, NuovoTentativo),
        (
            memberchk(Lettera, ParolaSegreta) ->
            gioca(ParolaSegreta, NuovoTentativo, TentativiRimanenti)
            ;
            NuoviTentativi is TentativiRimanenti - 1,
            gioca(ParolaSegreta, NuovoTentativo, NuoviTentativi)
        )
    ).


/*Aggiorna la lista di lettere indovinate attualmente con una nuova lettera inserita dall'utente */
aggiornaTentativo(ParolaSegreta, TentativoAttuale, Lettera, NuovoTentativo) :-
    (   memberchk(Lettera, TentativoAttuale) ->
        /*La lettera è già stata indovinata, non fare nulla*/
        write('Lettera già inserita'), nl,
        NuovoTentativo = TentativoAttuale
    ;   memberchk(Lettera, ParolaSegreta) ->
        /* La lettera è corretta, aggiorna il tentativo*/
        write('Lettera presente nella parola'), nl,
        append(TentativoAttuale, [Lettera], TempTentativo),
        sort(TempTentativo, NuovoTentativo)
    ;   % La lettera è sbagliata, non aggiungerla alla lista di lettere corrette
        NuovoTentativo = TentativoAttuale
    ).

/*Cicla ricorsivamente la parola che gli viene passata e controlla se nella lista tentativo il carattere è presente, se lo è lo stampa, altrimenti stampa un _ */
visualizzaParola([], _).
visualizzaParola([C|Resto], Tentativo) :-
    (   memberchk(C, Tentativo) ->
        write(C), write(' ')
    ;   write('_ ')
    ),
    visualizzaParola(Resto, Tentativo).

/*Funzione che restituisce true quando produce una lista vuota, cioè quando tutte le lettere sono state indovinate*/
parolaIndovinata(ParolaSegreta, Tentativo) :-
    subtract(ParolaSegreta, Tentativo, []).

/* Predicato per disegnare il patibolo e l' omino */
disegnaPatibolo(TentativiRimanenti) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    caso(TentativiRimanenti),
    write('========='), nl.

/* Predicato ausiliario per gestire i casi del patibolo */
caso(6) :-
    write('      |'), nl,
    write('      |'), nl,
    write('      |'), nl.
caso(5) :-
    write('  O   |'), nl,
    write('      |'), nl,
    write('      |'), nl.
caso(4) :-
    write('  O   |'), nl,
    write('  |   |'), nl,
    write('      |'), nl.
caso(3) :-
    write('  O   |'), nl,
    write(' /|   |'), nl,
    write('      |'), nl.
caso(2) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write('      |'), nl.
caso(1) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' /    |'), nl.
caso(0) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' / \\  |'), nl.
caso(_).

/* Predicato per pulire la console */
pulisciConsole :-
    nl.

/* Predicato per stampare in maniera più bella ed organizzata una lista */
stampa_lista([]).
stampa_lista([X|Xs]) :-
    write(X),
    stampa_lista(Xs).

