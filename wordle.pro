% wordle.pro --- Play wordle with german words
%
% Author:  Stefan Möding <stm@kill-9.net>
% Created: <2022-03-29 18:37:22 stm>
% Updated: <2022-04-07 15:23:09 stm>
%

dynamic(wordle).


% weight(?atom, ?integer)
%
% weight(Letter, Weight) succeeds if Letter has Weight as determined by the
% letter frequency for German words.
%
weight(a, 558).
weight(b, 196).
weight(c, 316).
weight(d, 498).
weight(e, 1693).
weight(f, 149).
weight(g, 302).
weight(h, 498).
weight(i, 802).
weight(j, 24).
weight(k, 132).
weight(l, 360).
weight(m, 255).
weight(n, 1053).
weight(o, 224).
weight(p, 67).
weight(q, 2).
weight(r, 689).
weight(s, 642).
weight(t, 579).
weight(u, 383).
weight(v, 84).
weight(w, 178).
weight(x, 5).
weight(y, 5).
weight(z, 121).


% getweight(?list, ?integer)
%
% getweight(List, Weight) succeeds if the total weights for all letters in
% List is Weight. If a letter is used more than once in a word, only one
% occurrence is used to calculate the weight. This is used to prefer words
% with different letters over words where some letters are used more than
% once.
%
getweight([], 0).
getweight([H|T], Weight) :-
    weight(H, W1),
    delete(T, H, Rest),
    getweight(Rest, W2),
    Weight is W1 + W2.


% read_line_to_codes(+stream, ?list)
%
% read_line_to_codes(Stream, Codes) succeeds if Codes is the list of
% character codes that make up a line read from Stream.
%
%
read_line_to_codes(Stream, Codes) :-
    get_code(Stream, C0),
    (   C0 == -1
    ->  Codes0 = end_of_file
    ;   read_1line_to_codes(C0, Stream, Codes0)
    ),
    Codes = Codes0.

read_1line_to_codes(-1, _, []) :- !.
read_1line_to_codes(10, _, []) :- !.
read_1line_to_codes(13, Stream, L) :-
    !,
    get_code(Stream, C2),
    read_1line_to_codes(C2, Stream, L).

read_1line_to_codes(C, Stream, [C|T]) :-
    get_code(Stream, C2),
    read_1line_to_codes(C2, Stream, T).


% play
%
% play initializes the wordle(Word, Weight) fact for all words read from the
% file "wordle.txt". Word is an atom of the word and Weight the total sum of
% the weight of the characters that are contained in Word.
%
% play must be called to play a new game.
%
play :-
    randomize,
    retractall(wordle(_, _)),
    open('wordle.txt', read, Str),
    repeat,
    read_line_to_codes(Str, Line),
    atom_codes(Word, Line),
    atom_chars(Word, List),
    getweight(List, Weight),
    assertz(wordle(Word, Weight)),
    at_end_of_stream(Str),
    close(Str),
    !.

% write_words(+list)
%
% write_words(List) writes all elements of List as a side effect.
%
write_words([]).
write_words([H|T]) :- write(H), nl, write_words(T).


% words
% words(-list)
%
% words writes all Words for which wordle(Word, _) can be proved. It also
% writes the number of items found.
%
% words(Words) instantiates the variable Words with the list of words for
% which wordle(Word, _) can be proved.
%
words :-
    findall(X, wordle(X, _), Words),
    write_words(Words),
    length(Words, Count),
    write(Count),
    write(' candidates'),
    nl.

words(Words) :-
    findall(X, wordle(X, _), Words).


% gray(+atom)
%
% gray(Char) retracts all wordle(Word, _) facts that do not have Char as
% a character in Word.
%
gray(Char) :-
    findall(X, wordle(X, _), Words),
    gray(Char, Words),
    !.

gray(_, []).

gray(Char, [H|T]) :-
    gray1(Char, H),
    gray(Char, T).

gray1(Char, Word) :-
    atom_chars(Word, List),
    memberchk(Char, List),
    retract(wordle(Word, _)).

gray1(Char, Word) :-
    atom_chars(Word, List),
    \+ memberchk(Char, List).


% green(+atom, +integer)
%
% green(Char, Pos) retracts all wordle(Word, _) facts that do not have Char
% as a character at position Pos in Word.
%
green(Char, Pos) :-
    findall(X, wordle(X, _), Words),
    green(Char, Pos, Words),
    !.

green(_, _, []).

green(Char, Pos, [H|T]) :-
    green1(Char, Pos, H),
    green(Char, Pos, T).

green1(Char, Pos, Word) :-
    atom_chars(Word, List),
    nth(Pos, List, Wordchar),
    Char \= Wordchar,
    retract(wordle(Word, _)).

green1(Char, Pos, Word) :-
    atom_chars(Word, List),
    nth(Pos, List, Wordchar),
    Char = Wordchar.


% yellow(+atom, +integer)
%
% yellow(Char, Pos) retracts all wordle(Word, _) that either have Char at
% position Pos in Word or do not have Char at one of the other positions.
%
yellow(Char, Pos) :-
    findall(X, wordle(X, _), Words),
    yellow(Char, Pos, Words),
    !.

yellow(_, _, []).

yellow(Char, Pos, [H|T]) :-
    yellow1(Char, Pos, H),
    yellow(Char, Pos, T).

yellow1(Char, Pos, Word) :-
    atom_chars(Word, List),
    nth(Pos, List, Wordchar),
    Char = Wordchar,
    retract(wordle(Word, _)).

yellow1(Char, Pos, Word) :-
    atom_chars(Word, List),
    nth(Pos, List, Wordchar),
    Char \= Wordchar,
    \+ memberchk(Char, List),
    retract(wordle(Word, _)).

yellow1(Char, Pos, Word) :-
    atom_chars(Word, List),
    nth(Pos, List, Wordchar),
    Char \= Wordchar,
    memberchk(Char, List).


% bestguess(-list)
%
% bestguess(Words) instantiates Words with a list of Word for which
% wordle(Word, Weight) can be proved and Weight is the maximal weight.
%
bestguess(Words) :-
    findall(W, wordle(_, W), Weights),
    max_list(Weights, Max),
    findall(X, wordle(X, Max), Words).


% guess(-atom)
%
% guess(Word) instantiates Word with a random Word for which wordle(Word, _)
% can be proved.
%
guess(Word) :-
    findall(X, wordle(X, _), Words),
    length(Words, Len),
    random(1, Len, Pos),
    nth(Pos, Words, Word).
