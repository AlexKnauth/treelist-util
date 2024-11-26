#lang info

;; Package Info

(define collection "treelist-util")
(define deps
  '("base"
    ))

(define build-deps
  '("scribble-lib"
    "racket-doc"
    "rackunit-lib"))

(define pkg-desc "Treelist operations matching racket/list that aren't in racket/treelist")

(define version "0.0")

(define license 'MIT)

(define pkg-authors '(AlexKnauth))

;; Collection Info

(define scribblings '(("scribblings/treelist-util.scrbl" ())))
