/* Logical and Functional Programming Project a.y. 2023-2024
   Lecturer: Prof. Marco Bernardo
   Students: 
            Tommaso Remedi  - 300535 
            Riccardo Monaco - 300537 */

/* Prolog program to play the hangman game */

main :- 
    env_setup(6).

/* Words to guess list */

words_list(['haskell', 'programmazione', 'funzionale', 'linguaggio', 'computazione']).

/* The predicate env_setup sets the game environment, picking the word to guess
   - The first parameter stands for the total attempts */

env_setup(Remaining_Attemps) :- 
    clean_console,
    write('Welcome to the Hangman Game!'), nl,
    sel_word(Word_To_Guess),
    play(Word_To_Guess, [], Remaining_Attemps).

/* The predicate sel_world randomly selects a word from the given list 
   - The first parameter stands for the word to be selected */

sel_word(Word) :-
    words_list(Words_List),
    length(Words_List, List_Length),
    random(0, List_Length, Word_Index),
    nth0(Word_Index, Words_List, Atom_Word),
    atom_chars(Atom_Word, Word).

/* The predicate play manages the game, it updates the guessed letters and the attempts 
   - The first parameter stands for the word to guess
   - The second parameter stands for the letters already guessed
   - The third parameter stands for the remaining attempts */

play(Word_To_Guess, Guessed_Letters, Remaining_Attemps) :-
    Remaining_Attemps =:= 0 ->
        handle_loss(Word_To_Guess)
    ;
    check_guessed(Word_To_Guess, Guessed_Letters) ->
        handle_win(Word_To_Guess)
    ;   
        handle_in_progress(Word_To_Guess, Guessed_Letters, Remaining_Attemps).

handle_loss(Word_To_Guess) :-
    clean_console,
    draw_hangman(0),
    write('You\'ve Lost!'), nl,
    write('The word to guess was: '), print_list(Word_To_Guess), nl.

handle_win(Word_To_Guess) :-
    clean_console,
    render_word(Word_To_Guess, Guessed_Letters), nl,
    write('You\'ve won! The secret word was: '), print_list(Word_To_Guess), nl.

handle_in_progress(Word_To_Guess, Guessed_Letters, Remaining_Attemps) :-
    clean_console,
    write('Actual word:'), nl,
    render_word(Word_To_Guess, Guessed_Letters), nl, nl,
    draw_hangman(Remaining_Attemps),
    write('Remaining attempts: '), write(Remaining_Attemps), nl, nl,
    write('Guess a Letter: '), nl,
    read_first_char(Inserted_Letter),
    upd_attempt(Word_To_Guess, Guessed_Letters, Inserted_Letter, Remaining_Attemps).


/* Predicato principale che legge il primo carattere e il resto della stringa*/
read_first_char(FirstChar) :-
    /* Legge il codice ASCII del primo carattere*/
    get_code(FirstCode),
    /* Converte il codice ASCII in char  */
    atom_codes(FirstChar, [FirstCode]),
    (
        (FirstCode >= 97, FirstCode =< 122) ->
            /* Chiama il predicato ausiliario per svuotare il resto della stringa*/
            read_remaining_chars(FirstCode, RestOfStringCodes)
            
            
        ;
            /* Altrimenti, richiede un nuovo input */
            write('Per favore, inserisci una lettera minuscola valida.'), nl,
            /* Chiama il predicato ausiliario per svuotare il resto della stringa*/
            read_remaining_chars(FirstCode, RestOfStringCodes),
            clean_console,
            write('Guess a Letter: '), nl,
            read_first_char(Inserted_Letter)
            
    ).


/* Predicato ausiliario che legge il resto della stringa carattere per carattere*/
read_remaining_chars(10, []) :- !.  

read_remaining_chars(Code, [Code | Rest]) :-
    /* Legge il codice ASCII del carattere successivo*/
    get_code(NewCode),
    /* Chiama ricorsivamente il predicato ausiliario con il nuovo codice ASCII*/
    read_remaining_chars(NewCode, Rest).


/* The predicate upd_attempt updates the guessed Letters list with eventually a new one 
   - The first parameter stands for the word to guess
   - The second parameter stands for the letters already guessed
   - The third parameter stands for the letter the user has inserted
   - The fourth parameter stands for the remaining attempts */

upd_attempt(Word_To_Guess, Guessed_Letters, Inserted_Letter, Remaining_Attemps) :-
    write('____________________________________'), nl, nl,
    memberchk(Inserted_Letter, Guessed_Letters) ->
           write('Already guessed letter'), nl,
           play(Word_To_Guess, Guessed_Letters, Remaining_Attemps)                 
    ;   
    memberchk(Inserted_Letter, Word_To_Guess) ->
           write('Letter is in the word!'), nl,
           append(Guessed_Letters, [Inserted_Letter], Upd_Guessed_Letters),
           play(Word_To_Guess, Upd_Guessed_Letters, Remaining_Attemps)
    ;
    write('Wrong letter!'), nl,
    Upd_Remaining_Attempts is Remaining_Attemps - 1,
    play(Word_To_Guess, Guessed_Letters, Upd_Remaining_Attempts).


/* The predicate render_word prints a letter if it is found in the word, a "_" if not 
   - The first parameter stands for the letters of the word to guess
   - The second parameter stands for the letters already guessed */

render_word([], _).
render_word([C|Word_To_Guess], Guessed_Letters) :-
    (
        memberchk(C, Guessed_Letters) ->
        write(C), write(' ')
        ;   
        write('_ ')
    ),
    render_word(Word_To_Guess, Guessed_Letters).

/* The predicate check_guessed returns true if all the letters have been guessed 
   - The first parameter stands for the word to guess
   - The second parameter stands for the letters already guessed */

check_guessed(Word_To_Guess, Guessed_Letters) :-
    subtract(Word_To_Guess, Guessed_Letters, []).

/* The predicate draw_hangman prints the characters to draw the countours of the hangman 
   - The first parameter stands for the remaining attempts */

draw_hangman(Remaining_Attemps) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    draw_case(Remaining_Attemps),
    write('========='), nl.

/* The auxiliary predicate draw_case draws the specific "state" of the hangman 
   - The first parameter stands for the reached level of error */

draw_case(6) :-
    write('      |'), nl,
    write('      |'), nl,
    write('      |'), nl.
draw_case(5) :-
    write('  O   |'), nl,
    write('      |'), nl,
    write('      |'), nl.
draw_case(4) :-
    write('  O   |'), nl,
    write('  |   |'), nl,
    write('      |'), nl.
draw_case(3) :-
    write('  O   |'), nl,
    write(' /|   |'), nl,
    write('      |'), nl.
draw_case(2) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write('      |'), nl.
draw_case(1) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' /    |'), nl.
draw_case(0) :-
    write('  O   |'), nl,
    write(' /|\\  |'), nl,
    write(' / \\  |'), nl.

/* The predicate clean_console prints a new line */

clean_console :-
    nl.

/* The predicate print_list pretty prints a list 
   - The first parameter stands for the list to print */

print_list([]).
print_list([X|Xs]) :-
    write(X),
    print_list(Xs).