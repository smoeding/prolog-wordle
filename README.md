# Play Wordle using GNU Prolog

This is an implementation of the [Wordle](https://en.m.wikipedia.org/wiki/Wordle) game written in [GNU Prolog](http://www.gprolog.org).  The program tries to guess German words and expects updates to the knowledge base about the answers. So it can help you to play a Wordle game (try https://wordle.at for German words).

## How to play

Start by consulting the code:

    | ?- [wordle].
    compiling /Users/stm/prolog-wordle/wordle.pro for byte code...
    /Users/stm/prolog-wordle/wordle.pro compiled, 235 lines read - 15126 bytes written, 7 ms

The initialize a new game by calling the `play` goal:

    | ?- play.

You can query the possible solutions by calling the `words` goal:

    | ?- words.
    ...
    zwick
    zwing
    zwirn
    zwist
    zyste
    4411 candidates

There are more than 4000 possible solutions when the game starts and no additional facts are added to the knowledge base.

Let's start and try the word *insel*. Maybe this guess tells us the letters *i*, *e* and *l* are yellow and the letters *n* and *s* are gray. So we enter this data into the knowledge base:

    | ?- yellow(i, 1).
    | ?- yellow(e, 4).
    | ?- yellow(l, 5).
    | ?- gray(n).
    | ?- gray(s).

Now we can look at the remaining possibilities:

    | ?- words.
    ...
    ylide
    zeile
    ziele
    zielt
    zille
    93 candidates

So only 93 words are left. Let's ask Prolog about the best guess:

    | ?- bestguess(Words).
    Words = [heilt,hielt,leiht,lieht,tilde]

The program suggests to try one of these words next. It uses a heuristic to prefer words with common letters and also words without duplicate letters.

Let's go for *tilde*. Now we might get the feedback, that *t* and *d* are gray, *i* and *e* are still only yellow but *l* is green. So we enter the new data into the knowledge base:

    | ?- gray(t).
    | ?- green(l, 3).
    | ?- gray(d).
    | ?- yellow(e, 5).

Then we check the remaining solution candidates:

    | ?- words.
    celli
    eilig
    eklig
    oelig
    4 candidates

As you can see we are down to only 4 remaining words.

## Other languages

The file `wordle.txt` contains German words that are loaded when you start to play a new game. You can try to replace the file with a list of words from a different language. The heuristic for the best guess uses the distribution of letters in German words. The given distribution might work for some languages better than for others.
