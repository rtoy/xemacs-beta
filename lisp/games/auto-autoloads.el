;;; DO NOT MODIFY THIS FILE
(if (featurep 'games-autoloads) (error "Already loaded"))

;;;### (autoloads (blackbox) "blackbox" "games/blackbox.el")

(autoload 'blackbox "blackbox" "\
Play blackbox.  Optional prefix argument is the number of balls;
the default is 4.

What is blackbox?

Blackbox is a game of hide and seek played on an 8 by 8 grid (the
Blackbox).  Your opponent (Emacs, in this case) has hidden several
balls (usually 4) within this box.  By shooting rays into the box and
observing where they emerge it is possible to deduce the positions of
the hidden balls.  The fewer rays you use to find the balls, the lower
your score.

Overview of play:

\\<blackbox-mode-map>To play blackbox, type \\[blackbox].  An optional prefix argument
specifies the number of balls to be hidden in the box; the default is
four.

The cursor can be moved around the box with the standard cursor
movement keys.

To shoot a ray, move the cursor to the edge of the box and press SPC.
The result will be determined and the playfield updated.

You may place or remove balls in the box by moving the cursor into the
box and pressing \\[bb-romp].

When you think the configuration of balls you have placed is correct,
press \\[bb-done].  You will be informed whether you are correct or
not, and be given your score.  Your score is the number of letters and
numbers around the outside of the box plus five for each incorrectly
placed ball.  If you placed any balls incorrectly, they will be
indicated with `x', and their actual positions indicated with `o'.

Details:

There are three possible outcomes for each ray you send into the box:

	Detour: the ray is deflected and emerges somewhere other than
		where you sent it in.  On the playfield, detours are
		denoted by matching pairs of numbers -- one where the
		ray went in, and the other where it came out.

	Reflection: the ray is reflected and emerges in the same place
		it was sent in.  On the playfield, reflections are
		denoted by the letter `R'.

	Hit:	the ray strikes a ball directly and is absorbed.  It does
		not emerge from the box.  On the playfield, hits are
		denoted by the letter `H'.

The rules for how balls deflect rays are simple and are best shown by
example.

As a ray approaches a ball it is deflected ninety degrees.  Rays can
be deflected multiple times.  In the diagrams below, the dashes
represent empty box locations and the letter `O' represents a ball.
The entrance and exit points of each ray are marked with numbers as
described under \"Detour\" above.  Note that the entrance and exit
points are always interchangeable.  `*' denotes the path taken by the
ray.

Note carefully the relative positions of the ball and the ninety
degree deflection it causes.

    1                                            
  - * - - - - - -         - - - - - - - -         - - - - - - - -       
  - * - - - - - -         - - - - - - - -         - - - - - - - -       
1 * * - - - - - -         - - - - - - - -         - O - - - - O -       
  - - O - - - - -         - - O - - - - -         - - * * * * - -
  - - - - - - - -         - - - * * * * * 2     3 * * * - - * - -
  - - - - - - - -         - - - * - - - -         - - - O - * - -      
  - - - - - - - -         - - - * - - - -         - - - - * * - -       
  - - - - - - - -         - - - * - - - -         - - - - * - O -       
                                2                         3

As mentioned above, a reflection occurs when a ray emerges from the same point
it was sent in.  This can happen in several ways:

                                                                           
  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - O - - -         - - O - O - - -          - - - - - - - -
R * * * * - - - -         - - - * - - - -          O - - - - - - -
  - - - - O - - -         - - - * - - - -        R - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -         - - - * - - - -          - - - - - - - -
  - - - - - - - -       R * * * * - - - -          - - - - - - - -
  - - - - - - - -         - - - - O - - -          - - - - - - - -

In the first example, the ray is deflected downwards by the upper
ball, then left by the lower ball, and finally retraces its path to
its point of origin.  The second example is similar.  The third
example is a bit anomalous but can be rationalized by realizing the
ray never gets a chance to get into the box.  Alternatively, the ray
can be thought of as being deflected downwards and immediately
emerging from the box.

A hit occurs when a ray runs straight into a ball:

  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - O - - -
  - - - - - - - -         - - - - O - - -        H * * * * - - - -
  - - - - - - - -       H * * * * O - - -          - - - * - - - -
  - - - - - - - -         - - - - O - - -          - - - O - - - -
H * * * O - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -
  - - - - - - - -         - - - - - - - -          - - - - - - - -

Be sure to compare the second example of a hit with the first example of
a reflection." t nil)

;;;***

;;;### (autoloads (conx-load conx conx-region conx-buffer) "conx" "games/conx.el")

(autoload 'conx-buffer "conx" "\
Absorb the text in the current buffer into the tree." t nil)

(autoload 'conx-region "conx" "\
Absorb the text in the current region into the tree." t nil)

(autoload 'conx "conx" "\
Generate some random sentences in the *conx* buffer." t nil)

(autoload 'conx-load "conx" "\
Load in a CONX database written by the \\[conx-save] command.
This clears the database currently in memory." t nil)

;;;***

;;;### (autoloads (shuffle-vector cookie-snarf cookie-insert cookie) "cookie1" "games/cookie1.el")

(autoload 'cookie "cookie1" "\
Return a random phrase from PHRASE-FILE.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end." nil nil)

(autoload 'cookie-insert "cookie1" "\
Insert random phrases from PHRASE-FILE; COUNT of them.  When the phrase file
is read in, display STARTMSG at beginning of load, ENDMSG at end." nil nil)

(autoload 'cookie-snarf "cookie1" "\
Reads in the PHRASE-FILE, returns it as a vector of strings.
Emit STARTMSG and ENDMSG before and after.  Caches the result; second
and subsequent calls on the same file won't go to disk." nil nil)

(autoload 'shuffle-vector "cookie1" "\
Randomly permute the elements of VECTOR (all permutations equally likely)" nil nil)

;;;***

;;;### (autoloads (decipher-mode decipher) "decipher" "games/decipher.el")

(autoload 'decipher "decipher" "\
Format a buffer of ciphertext for cryptanalysis and enter Decipher mode." t nil)

(autoload 'decipher-mode "decipher" "\
Major mode for decrypting monoalphabetic substitution ciphers.
Lower-case letters enter plaintext.
Upper-case letters are commands.

The buffer is made read-only so that normal Emacs commands cannot
modify it.

The most useful commands are:
\\<decipher-mode-map>
\\[decipher-digram-list]  Display a list of all digrams & their frequency
\\[decipher-frequency-count]  Display the frequency of each ciphertext letter
\\[decipher-adjacency-list]  Show adjacency list for current letter (lists letters appearing next to it)
\\[decipher-make-checkpoint]  Save the current cipher alphabet (checkpoint)
\\[decipher-restore-checkpoint]  Restore a saved cipher alphabet (checkpoint)" t nil)

;;;***

;;;### (autoloads (dissociated-press) "dissociate" "games/dissociate.el")

(autoload 'dissociated-press "dissociate" "\
Dissociate the text of the current buffer.
Output goes in buffer named *Dissociation*,
which is redisplayed each time text is added to it.
Every so often the user must say whether to continue.
If ARG is positive, require ARG chars of continuity.
If ARG is negative, require -ARG words of continuity.
Default is 2." t nil)

;;;***

;;;### (autoloads (doctor) "doctor" "games/doctor.el")

(autoload 'doctor "doctor" "\
Switch to *doctor* buffer and start giving psychotherapy." t nil)

;;;***

;;;### (autoloads (dunnet) "dunnet" "games/dunnet.el")

(autoload 'dunnet "dunnet" "\
Switch to *dungeon* buffer and start game." t nil)

;;;***

;;;### (autoloads (flame) "flame" "games/flame.el")

(autoload 'flame "flame" "\
Generate ARG (default 1) sentences of half-crazed gibberish." t nil)

;;;***

;;;### (autoloads (gomoku) "gomoku" "games/gomoku.el")

(autoload 'gomoku "gomoku" "\
Start a Gomoku game between you and Emacs.
If a game is in progress, this command allow you to resume it.
If optional arguments N and M are given, an N by M board is used.

You and Emacs play in turn by marking a free square. You mark it with X
and Emacs marks it with O. The winner is the first to get five contiguous
marks horizontally, vertically or in diagonal.
You play by moving the cursor over the square you choose and hitting
\\<gomoku-mode-map>\\[gomoku-human-plays].
Use \\[describe-mode] for more info." t nil)

;;;***

;;;### (autoloads (hanoi) "hanoi" "games/hanoi.el")

(autoload 'hanoi "hanoi" "\
Towers of Hanoi diversion.  Argument is number of rings." t nil)

;;;***

;;;### (autoloads (life) "life" "games/life.el")

(autoload 'life "life" "\
Run Conway's Life simulation.
The starting pattern is randomly selected.  Prefix arg (optional first
arg non-nil from a program) is the number of seconds to sleep between
generations (this defaults to 1)." t nil)

;;;***

;;;### (autoloads (mpuz) "mpuz" "games/mpuz.el")

(autoload 'mpuz "mpuz" "\
Multiplication puzzle with GNU Emacs." t nil)

;;;***

;;;### (autoloads (snarf-spooks spook) "spook" "games/spook.el")

(autoload 'spook "spook" "\
Adds that special touch of class to your outgoing mail." t nil)

(autoload 'snarf-spooks "spook" "\
Return a vector containing the lines from `spook-phrases-file'." nil nil)

;;;***

;;;### (autoloads (tetris) "tetris" "games/tetris.el")

(autoload 'tetris "tetris" "\
Tetris

Shapes drop from the top of the screen, and the user has to move and
rotate the shape to fit in with those at the bottom of the screen so
as to form complete rows.

tetris-mode keybindings:
   \\<tetris-mode-map>
\\[tetris-start-game]	Starts a new game of Tetris
\\[tetris-end-game]	Terminates the current game
\\[tetris-move-left]	Moves the shape one square to the left
\\[tetris-move-right]	Moves the shape one square to the right
\\[tetris-rotate-prev]	Rotates the shape clockwise
\\[tetris-rotate-next]	Rotates the shape anticlockwise
\\[tetris-move-bottom]	Drops the shape to the bottom of the playing area

" t nil)

;;;***

;;;### (autoloads (xmine-mode) "xmine" "games/xmine.el")

(autoload 'xmine-mode "xmine" "\
A mode for playing the well known mine searching game.

   `\\<annotation-local-map-default>\\[xmine-activate-function-button1]' or `\\<xmine-keymap>\\[xmine-key-action1]' unhides a tile,
   `\\<annotation-local-map-default>\\[xmine-activate-function-button2]' or `\\<xmine-keymap>\\[xmine-key-action2]' unhides all neighbours of a tile,
   `\\<annotation-local-map-default>\\[xmine-activate-function-button3]' or `\\<xmine-keymap>\\[xmine-key-action3]' (un)flagges a tile to hold a mine.

   `\\[xmine-key-new]' starts a new game.
   `\\[xmine-key-quit]' ends a game.

All keybindings (with alternatives) currently in effect:
   \\{xmine-keymap}

The rules are quite easy: You start by unhiding (random) tiles. An unhidden
tile showing a number tells you something about the number of mines in it's
neighborhood, where the neighborhood are all 8 tiles (or less if it's
at a border) around the tile.

E.g. a \"1\" shows you that there is only one mine in the neighborhood of
this tile. Empty tiles have no mines around them, and empty tiles in
the neighborhood of another empty tile are all automatically unhidden
if you unhide one of them. You need to find a strategy to use the
information you have from the numbers to \"flag\" the tiles with mines
under them and unhide all other tiles. If you correctly made this
without accidently unhiding a mine, you've won.

If you are sure you have correctly flagged all mines around a unhidden tile,
you can use Button-2 or \\[xmine-key-action2] on it to unhide all it's
neighbors. But beware: If you made a mistake by flagging the wrong mines,
you'll blow up! 

Have Fun." t nil)

(fset 'xmine 'xmine-mode)

;;;***

;;;### (autoloads (psychoanalyze-pinhead apropos-zippy insert-zippyism yow) "yow" "games/yow.el")

(autoload 'yow "yow" "\
Return or display a random Zippy quotation.  With prefix arg, insert it." t nil)

(autoload 'insert-zippyism "yow" "\
Prompt with completion for a known Zippy quotation, and insert it at point." t nil)

(autoload 'apropos-zippy "yow" "\
Return a list of all Zippy quotes matching REGEXP.
If called interactively, display a list of matches." t nil)

(autoload 'psychoanalyze-pinhead "yow" "\
Zippy goes to the analyst." t nil)

;;;***

(provide 'games-autoloads)
