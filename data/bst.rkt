#lang racket/base

; Binary Search Tree ADT
; ======================
;
; A binary search tree is almost never used
; directly. Instead, it serves as a powerfull
; implementation mechanism for other ADT's.
;
; Using linked lists, all mutations and lookups
; now have a worst-case of O(log n) at most.

(require racket/class)
