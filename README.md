# Play Wordle using GNU Prolog

This is an implementation of the [Wordle](https://en.m.wikipedia.org/wiki/Wordle) game written in [GNU Prolog](http://www.gprolog.org).  The program tries to guess English or German words and expects updates to the knowledge base about the answers. So it can help you to play a Wordle game (try https://wordle.at for German words).

## How to play

Start by consulting the code:

    | ?- [wordle].
    compiling /Users/stm/prolog-wordle/wordle.pro for byte code...
    /Users/stm/prolog-wordle/wordle.pro compiled, 235 lines read - 15126 bytes written, 7 ms

Initialize a new game by calling the `play` goal with the language to use as parameter. Currently only the values `english` and `german` are supported.

    | ?- play(german).

You can list the possible solutions by calling the `words` goal:

    | ?- words.
    ...
    zwick
    zwing
    zwirn
    zwist
    zyste
    4411 candidates

With the provided list of German words there are more than 4000 possible solutions when the game starts and no additional facts are added to the knowledge base.

Let's start and try the word *insel*. Maybe this guess tells us the letters *i*, *e* and *l* are yellow and the letters *n* and *s* are gray. So we enter this data into the knowledge base:

    | ?- yellow(i, 1), gray(n), gray(s), yellow(e, 4), yellow(l, 5).

Now we can look at the remaining possibilities:

    | ?- words.
    ...
    ylide
    zeile
    ziele
    zielt
    zille
    93 candidates

Now only 93 words are left. Let's ask Prolog about the best guess:

    | ?- bestguess(Words).
    Words = [heilt,hielt,leiht,lieht,tilde]

The program suggests to try one of these words next. It uses the following heuristic:
* prefer words with common letters over words with rarely used letters
* prefer words with distinct letters over words with duplicate letters

Let's go for *tilde*. Now we might get the feedback, that *t* and *d* are gray, *i* and *e* are still only yellow but *l* is green. So we enter the new data into the knowledge base:

    | ?- gray(t), yellow(i, 2), green(l, 3), gray(d), yellow(e, 5).

Then we check the remaining solution candidates:

    | ?- words.
    celli
    eklig
    oelig
    3 candidates

As you can see we are down to only 3 remaining words after two guesses.

## Implemented goals

In the following descriptions `Position` and `Number` must be an integer between `1` and `5` and `Letter` must be a single character atom.

### play(+atom)

`play(Language)` starts a new game. All known words for the given `Language` are loaded from a file and established as facts. Currently `Language` must be the atom `english` or `german` to load the list of words.

The goals `english` or `german` can be used as a shortcut.

### green(+atom, +integer)

`green(Letter, Position)` marks `Letter` as correct on `Position`.

The clause removes all words that have a different letter on the given position from the knowledge base. So effectively only words that have the letter at this position are kept in the knowledge base for subsequent searches.

### yellow(+atom, +integer)

`yellow(Letter, Position)` marks `Letter` as used in the solution but not on `Position`.

The clause removes all words from the knowledge base that either do not include `Letter` at all or have `Letter` on `Position`.

### gray(+atom, +integer)

`gray(Letter, Number)` marks `Letter` as not used in the solution if the letter occurs `Number` or more times.

The goal removes all words from the knowledge base where `Letter` occurs `Number` or more times.

The parameter `Number` should be instantiated with `1` for a letter that is only tested once and receives the gray mark.

`Number` should be instantiated with `2` or a higher number to remove all words where the letter occurs more than the given number. This is needed if you try a word where a specific letter is used multiple times and the result shows different colors for the letter (e.g. once yellow and once gray).

### words

`words` prints a list of remaining words to the terminal.

### bestguess(-list)

`bestguess(Words)` instantiates `Words` with a list of words that remain as solution candidates. It uses a heuristic that prefers words with common letters.

### bestguess1(-atom)

`bestguess1(Word)` instantiates `Word` with a single random word that remain as solution candidates. It uses `bestguess(Words)` to produce a list of words and selects a random word from that list.

# Compilation

You can compile the program into a native executable using the GNU Prolog compiler:

    gplc wordle.pro
