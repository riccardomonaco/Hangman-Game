/* Prolog program to play the hangman game */

main :- env_setup(6).

/* Words to guess list */

words_list(['haskell', 'programmazione', 'funzionale', 'linguaggio', 'computazione']).

/* The predicate sel_world randomly selects a word from the given list */

sel_word(Word) :- words_list(Words_List),
                  length(Words_List, List_Length),
                  random(0, List_Length, Word_Index),
                  nth0(Word_Index, Words_List, Atomic_Word),
                  atom_chars(Atomic_Word, Word).

/* The predicate env_setup sets the game environment 
   - Welcomes the player
   - Draws the hangman
   - Selects the word to guess
   - Calls the play predicate to begin the game */

env_setup(Remaining_Attemps) :- clean_console,
                                write('Welcome to the Hangman Game!'), nl,
                                sel_word(Word_To_Guess),
                                play(Word_To_Guess, [], Remaining_Attemps).

/* The predicate play manages the game, it updates the guessed letters and the attempts 
   - The first parameter stands for the word to guess
   - The second parameter stands for the actual Letters guessed 
   - The third parameter stands for the remaining attempts */

play(Word_To_Guess, Actual_Attempt, Remaining_Attemps) :-
    (   Remaining_Attemps =:= 0 ->
        clean_console,
        draw_hangman(0),
        write('You\'ve Lost!'), nl,
        write('The secret word was: '),write_list(Word_To_Guess), nl
        ;   
        check_guessed(Word_To_Guess, Actual_Attempt) ->
        clean_console,
        render_word(Word_To_Guess, Actual_Attempt), nl,
        write('You\'ve won! The secret word was: '), print_list(Word_To_Guess), nl
        ;   
        clean_console,
        write('Actual word:'), nl,
        render_word(Word_To_Guess, Actual_Attempt), nl, nl,
        draw_hangman(Remaining_Attemps),
        write('Remaining attempts: '), write(Remaining_Attemps), nl, nl,
        write('Guess a Letter: '), nl,
        get_char(Letter),
        get_char(_),
        upd_attempt(Word_To_Guess, Actual_Attempt, Letter, New_Attempt),
        (
            memberchk(Letter, Word_To_Guess) ->
            play(Word_To_Guess, New_Attempt, Remaining_Attemps)
            ;
            New_Attempts is Remaining_Attemps - 1,
            play(Word_To_Guess, New_Attempt, New_Attempts)
        )
    ).

/* The predicate upd_attempt updates the guessed Letters list with eventually a new one 
   - The first parameter stands for the word to guess
   - The second parameter stands for the actual Letters guessed
   - The third parameter stands for the eventual new attempt */
   
upd_attempt(Word_To_Guess, Actual_Attempt, Letter, New_Attempt) :-
    (   memberchk(Letter, Actual_Attempt) ->
        write('Already guessed letter'), nl,
        New_Attempt = Actual_Attempt
        ;   
        memberchk(Letter, Word_To_Guess) ->
        write('Letter is in the word!'), nl,
        append(Actual_Attempt, [Letter], Temp_Attempt),
        sort(Temp_Attempt, New_Attempt)
        ;  
        New_Attempt = Actual_Attempt
    ).

/* The predicate render_word prints a letter if it is found in the word, a "_" if not */

render_word([], _).
render_word([C|Rest], Attempt) :-
    (   memberchk(C, Attempt) ->
        write(C), write(' ')
        ;   
        write('_ ')
    ),
    render_word(Rest, Attempt).

/* The predicate check_guessed returns true if all the letters have been guessed */

check_guessed(Word_To_Guess, Attempt) :-
    subtract(Word_To_Guess, Attempt, []).

/* The predicate draw_hangman prints the characters to draw the countours of the hangman */

draw_hangman(Remaining_Attemps) :-
    write('  +---+'), nl,
    write('  |   |'), nl,
    draw_case(Remaining_Attemps),
    write('========='), nl.

/* The auxiliary predicate draw_case draws the specifica "state" of the hangman */

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
draw_case(_).

/* The predicate clean_console  */
clean_console :-
    nl.

/* Predicato per stampare in maniera più bella ed organizzata una lista */
print_list([]).
print_list([X|Xs]) :-
    write(X),
    print_list(Xs).

