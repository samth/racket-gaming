Simple Racket Game Library
==========================

This is a small library written in [Racket](https://racket-lang.org) that aims to make developing
games within Racket more convenient.

This library is excellent for students just starting out with Racket and
programming in general, while it might also be useful to more experienced
programmers who want to quickly prototype a game. The
[issues](https://github.com/samvv/racket-gaming/labels/question) can be used if
you run into any problems.

## Breaking Changes

This library will be refactored in the upcoming days or weeks. To learn more, read the
[change log](https://github.com/samvv/racket-gaming/tree/master/CHANGELOG.md).

## Features ##

 - Fully integrated physics engine
 - Buffered frames preventing flicker

More to come soon!

## Installation

Ensure you have [Racket](https://racket-lang.org) installed, open up a
[terminal](https://help.ubuntu.com/community/UsingTheTerminal), and run the
following command:

```bash
$ raco install gaming
```

If you have problems feel free to open an issue.

**Hint:** if you're upgrading, make sure you read the
[changelog](https://github.com/samvv/racket-gaming/tree/master/CHANGELOG.md)
before doing so!

## Usage

```racket
#lang racket

(define circle-radius 25)

(require gaming)

(define the-screen
  (new screen%
       [title "My First Game!"]
       [width 600]
       [height 600]))

(define the-world 
  (new world% 
       [screen the-screen]
       [gravity (make-point 0 9.81)]))

(define the-ground
  (new segment%
       [a (make-point 0 (- (send the-screen get-height) (* circle-radius 2)))]
       [b (make-point (send the-screen get-width) (- (send the-screen get-height) (* circle-radius 2)))]
       [elasticity 1.0]
       [friction 1.0]))

(send the-world add-shapes the-ground)

(define falling-circle
  (new game-object%
       [world the-world]
       [position (make-point 300 300)]
       [image (circle circle-radius "solid" "blue")]
       [shape
         (new circle%
              [radius circle-radius]
              [elasticity 1.0]
              [friction 1.0])]))

(send the-world play)
```

You should see a blue circle falling and bouncing off the ground.

### Next steps

Please consult the
[full examples](https://github.com/samvv/racket-gaming/tree/master/examples) to get
your feet wet and to get an idea how the programs are made. Next, read
the documentation for extensive information on how to use the library. If you're stuck,
just [open an issue at GitHub](https://github.com/samvv/racket-gaming/issues/new).

## Credits

Library created and maintained by Sam Vervaeck.

Thanks to Adriaan Leynse for the original concept and
providing an exemplary implementation using racket/gui.

This library is a fork of the library provided by the
[Software Languages Lab](http://soft.vub.ac.be/soft/edu/teaching) as
part of one of their courses. A full backport to this library
has been provided in *canvas.rkt*. Use *graphics/canvas.rkt*  in
order to use racket-gaming as a back-en instead of the old library.
 
The libraries that have been of use are stored in the folder *references*.
