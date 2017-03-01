Change Log
==========

All notable changes to this project will be documented in this file.

## v0.2.0

This version is a major update and contains many breaking changes with the
previous version.

 - Created a complete new API using `racket/class`
 - Rendering now requires `2htdp/image`
 - Added [ramunk](https://github.com/samvv/ramunk) as the physics engine
 - Removed all legacy `canvas.rkt` code
 - Removed all procedural ("functional") code and made it an issue as a potential side-project
 - Created the basis for a new "ball pool" example

## v0.1.0

Legacy version containing improvements over the original library by Software Language Lab.

 - Added event-driven device definitions
 - Created a small physics engine
 - Created a custom API using dispatching
 - Added backwards-compatible `canvas.rkt` compliant with upstream

