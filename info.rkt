#lang info

;; Package Info

(define collection 'multi)

(define deps
  '("base"
    "reprovide-lang-lib"
    "rhombus-lib"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "rhombus"
    "rhombus-scribble-lib"
    "scribble-lib"))

(define pkg-desc
  "Treelist operations matching racket/list that aren't in racket/treelist")

(define version "0.0")

(define license 'MIT)

(define pkg-authors '(AlexKnauth))
