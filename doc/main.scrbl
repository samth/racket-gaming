#lang scribble/manual

@(require (for-label racket/base racket/class))

@title{Racket Game Library}

This package provides a basic framework to developing games in the Racket language.

@section["The World"]

A world represents a unique collection of game objects and defines the
relationships between these objects. The world can keep this information
in any way it chooses to, which allows for some critical performance 
optimizations.

@defclass[world% object% ()]{

 Represents one world. Usually, one instance of this object is created for each level in the game.
                             
@defmethod[(add-shape [shape (is-a? shape%)] ...) void?]{
  Adds a shape to the world itself. The shape will be attached to a virtual
  static game object, which is to say that none of the physical forces
  will act on the shape.

  This method is generally useful for adding impenetrable static structures,
  such as a fixed gound for left and right edges.
}

 @defmethod[(play) void?]{
  Start simulation of the world, meaning that physical forces will interact and the result will be drawn to the screen.
 }

 @defmethod[(pause) void?]{
  Pauses all physical forces and keeps the game standing still on the current frame.
 }

}

@section["Game Objects"]

 A game object is any object that takes part in the game dynamics in some way,
 either passively by being an uncontrolled obstacle,
 or actively by being controlled by the player.

@defclass[game-object% object% ()]{

 Represents an object in the game. The flag @code{controlled?} determines whether this
 game object is actively controlled by the player or just plays a passive role, being
 influenced by neighbouring objects and the physical forces.

 @defmethod[(get-x) number?]{
  Quickly get the x-coordinate of the position of this game object.
 }
 
 @defmethod[(get-x) number?]{
  Quickly get the y-coordinate of the position of this game object.
 }
 
 @defmethod[(get-position) void?]{
  Gets the current position of the game object.

  Note that this position might get updated by the physics engine.
 }

 @defmethod[(set-position [new-pos (is-a? point%)]) (is-a? point%)]{
  Sets the current position of the game object.

  Updating a game object's position is only safe for objects that have @code{controlled?} set to @code{#t}.
 }

 @defmethod[(get-velocity) void?]{
  Gets the current velocity of the game object.

  Note that this velocity might get updated by the physics engine.
 }

 @defmethod[(set-velocity [new-vel (is-a? point%)]) (is-a? point%)]{
  Sets the current velocity of the game object.

  Updating a game object's velocity is only safe for objects that have @code{controlled?} set to @code{#t}.
 }
 
}