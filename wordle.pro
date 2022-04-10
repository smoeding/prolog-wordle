% wordle.pro --- Play wordle with german or english words
%
% Author:  Stefan Möding <stm@kill-9.net>
% Created: <2022-03-29 18:37:22 stm>
% Updated: <2022-04-09 13:44:43 stm>
%

% wordle(?atom, ?integer)
%
% wordle(Word, Weight) succeeds if the letters used in Word have a total
% weight of Weight.
%
dynamic(wordle).


% weight(?atom, ?integer)
%
% weight(Char, Weight) succeeds if Char has Weight as determined by the
% letter frequency for the processed words.
%
dynamic(weight).


% incweight(+list)
%
% incweight(List) iterates over a list of single char atoms. For each char it
% checks if weight(Char, Weight) can be proved. If the clause weight(Char,
% Weight) exists, then the fact is retracted and a new fact with Weight+1 is
% asserted. Otherwise weight(Char, 1) is asserted.
%
incweight([]).

incweight([H|T]) :-
    clause(weight(H, Old), _),
    New is Old + 1,
    retract(weight(H, Old)),
    asserta(weight(H, New)),
    !,
    incweight(T).

incweight([H|T]) :-
    \+ clause(weight(H, _), _),
    asserta(weight(H, 1)),
    !,
    incweight(T).


% learn_letters(+list)
%
% learn_letters(List) iterates over a list of words. Each word is decomposed
% into a list of chars and the weight for these chars is incremented.
%
learn_letters([]).
learn_letters([H|T]) :-
    atom_chars(H, Chars),
    incweight(Chars),
    !,
    learn_letters(T).


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


% read_words(+stream, ?list)
%
% read_words(Stream, List) succeeds if List is a list of words read from
% Stream.
%
read_words(Stream, []) :-
    at_end_of_stream(Stream).

read_words(Stream, [H|T]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Line),
    atom_codes(H, Line),
    read_words(Stream, T),
    !.


% learn_words(+list)
%
% learn_words(List) iterates over List and adds wordle(Word, Weight) facts to
% the knowledge base as a side effect. Word is an atom of the word and Weight
% the total sum of the weight of the characters that are contained in Word.
%
learn_words([]).

learn_words([H|T]) :-
    atom_chars(H, Chars),
    getweight(Chars, Weight),
    assertz(wordle(H, Weight)),
    learn_words(T).


% play(+atom)
%
% play(Language) initializes the wordle(Word, Weight) fact for all words read
% from the file Language.
%
% play must be called to play a new game.
%
play(Language) :-
    randomize,
    % Forget all words and weights
    retractall(wordle(_, _)),
    retractall(weight(_, _)),
    % Read words from file
    open(Language, read, Str),
    read_words(Str, Words),
    close(Str),
    % Store weights for used letters in knowledge base
    learn_letters(Words),
    % Store words in knowledge base
    learn_words(Words).

german :- play(german).
english :- play(english).


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


% count(+atom, +list, ?integer)
%
% count(Element, List, Num) succeeds if Element occurs exactly Num times in
% List.
%
count(Element, List, 0) :-
    \+ memberchk(Element, List),
    !.

count(Element, List, Number) :-
    memberchk(Element, List),
    select(Element, List, List2),
    count(Element, List2, Remaining),
    Number is Remaining + 1,
    !.

% gray(+atom)
%
% gray(Char) retracts all wordle(Word, _) facts that do not have Char as
% a character in Word.
%
gray(Char) :- gray(Char, 1).


% gray(+atom, +integer)
%
% gray(Char, Count) retracts all wordle(Word, _) facts that do have Count or
% more occurences of Char. This is used if you have the same character marked
% as green/yellow and as gray. In this case you use gray(Char, 2) to indicate
% that the character is not used two or more times.
%
gray(Char, Count) :-
    findall(X, wordle(X, _), Words),
    gray(Char, Count, Words),
    !.

gray(_, _, []).

gray(Char, Count, [H|T]) :-
    gray1(Char, Count, H),
    gray(Char, Count, T).

gray1(Char, Count, Word) :-
    atom_chars(Word, List),
    count(Char, List, Num),
    Num >= Count,
    retract(wordle(Word, _)).

gray1(Char, Count, Word) :-
    atom_chars(Word, List),
    count(Char, List, Num),
    Num < Count.


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
% wordle(Word, Weight) can be proved and Weight is the maximum weight.
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
