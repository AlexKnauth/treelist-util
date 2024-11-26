#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/treelist
                     racket/mutable-treelist
                     treelist-util))

@title{treelist-util}

@author{Alex Knauth}

@(define the-eval (make-base-eval))
@(the-eval '(require racket/treelist
                     racket/mutable-treelist
                     racket/stream
                     treelist-util))

@defmodule[treelist-util]

Treelist operations matching @racketmodname[racket/list] that aren't in @racketmodname[racket/treelist].

@defproc[(treelist-index-where [tl treelist?] [pred (any/c . -> . any/c)])
         (or/c exact-nonnegative-integer? #f)]{

Returns the index of the first element in @racket[tl] where applying
@racket[pred] to the element produces a true value.
If no such element is found, the result is @racket[#f].

@examples[
#:eval the-eval
(define items (treelist 1 "a" 'apple))
(treelist-index-where items number?)
(treelist-index-where items string?)
(treelist-index-where items symbol?)
(treelist-index-where items void?)
]}

@defproc[(treelist-splitf [tl treelist?] [pred (any/c . -> . any/c)])
         (values treelist? treelist?)]{

Splits @racket[tl] into 2 treelists returned as values.
The first treelist contains elements taken successivly from @racket[tl]
as long as they satisfy @racket[pred].
The second treelist the rest of the elements of @racket[tl], from the
first element not satisfying @racket[pred] and onward.

@examples[
#:eval the-eval
(treelist-splitf (treelist 2 4 5 8) even?)
(treelist-splitf (treelist 2 4 5 8) odd?)
(treelist-splitf (treelist 2 4 6 8) even?)
(treelist-splitf (treelist 2 4 6 8) odd?)
]}
