#lang scribble/manual

@(require scribble/example
          (for-label racket/base
                     racket/contract
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

@deftogether[[
@defproc[(treelist* [v any/c] ... [tl treelist?]) treelist?]
@defproc[(build-treelist [n exact-nonnegative-integer?]
                         [proc (-> exact-nonnegative-integer? any/c)])
         treelist?]
@defproc[(treelist-andmap [proc procedure?] [tl treelist?] ...+) any/c]
@defproc[(treelist-ormap [proc procedure?] [tl treelist?] ...+) any/c]
@defproc[(treelist-foldl [proc procedure?] [init any/c] [tl treelist?] ...+) any/c]
@defproc[(treelist-foldr [proc procedure?] [init any/c] [tl treelist?] ...+) any/c]
@defproc[(treelist-remove [v any/c]
                          [tl treelist?]
                          [eql? (-> any/c any/c any/c) equal?])
         treelist?]
@defproc[(treelist-remq [v any/c] [tl treelist?]) treelist?]
@defproc[(treelist-remv [v any/c] [tl treelist?]) treelist?]
@defproc[(treelist-remw [v any/c] [tl treelist?]) treelist?]
@defproc[(treelist-remf [pred (-> any/c any/c)] [tl treelist?]) treelist?]
@defproc[(treelist-remove* [vs treelist?]
                           [tl treelist?]
                           [eql? (-> any/c any/c any/c) equal?])
         treelist?]
@defproc[(treelist-remq* [vs treelist?] [tl treelist?]) treelist?]
@defproc[(treelist-remv* [vs treelist?] [tl treelist?]) treelist?]
@defproc[(treelist-remw* [vs treelist?] [tl treelist?]) treelist?]
@defproc[(treelist-second [tl treelist?]) any/c]
@defproc[(treelist-third [tl treelist?]) any/c]
@defproc[(treelist-fourth [tl treelist?]) any/c]
@defproc[(treelist-fifth [tl treelist?]) any/c]
@defproc[(treelist-sixth [tl treelist?]) any/c]
@defproc[(treelist-seventh [tl treelist?]) any/c]
@defproc[(treelist-eighth [tl treelist?]) any/c]
@defproc[(treelist-ninth [tl treelist?]) any/c]
@defproc[(treelist-tenth [tl treelist?]) any/c]
@defproc[(treelist-eleventh [tl treelist?]) any/c]
@defproc[(treelist-twelfth [tl treelist?]) any/c]
@defproc[(treelist-thirteenth [tl treelist?]) any/c]
@defproc[(treelist-fourteenth [tl treelist?]) any/c]
@defproc[(treelist-fifteenth [tl treelist?]) any/c]
@defproc[(treelist-update [tl treelist?]
                          [pos exact-nonnegative-integer?]
                          [updater (-> any/c any/c)])
         treelist?]]]{
Operations similar to their counterparts from @racketmodname[racket/list].
}

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

@deftogether[[
@defproc[(treelist-indexes-of [tl treelist?]
                              [v any/c]
                              [eql? (-> any/c any/c any/c) equal?])
         treelist?]
@defproc[(treelist-indexes-where [tl treelist?] [pred (-> any/c any/c)]) treelist?]
@defproc[(treelist-split [tl treelist?] [pos exact-nonnegative-integer?])
         (values treelist? treelist?)]
@defproc[(treelist-takef [tl treelist?] [pred (-> any/c any/c)]) treelist?]
@defproc[(treelist-dropf [tl treelist?] [pred (-> any/c any/c)]) treelist?]]]{
Operations similar to their counterparts from @racketmodname[racket/list].
}

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

@deftogether[[
@defproc[(treelist-take-right [tl treelist?] [pos exact-nonnegative-integer?])
         treelist?]
@defproc[(treelist-drop-right [tl treelist?] [pos exact-nonnegative-integer?])
         treelist?]
@defproc[(treelist-split-right [tl treelist?] [pos exact-nonnegative-integer?])
         (values treelist? treelist?)]
@defproc[(treelist-takef-right [tl treelist?] [pred (-> any/c any/c)])
         treelist?]
@defproc[(treelist-dropf-right [tl treelist?] [pred (-> any/c any/c)])
         treelist?]
@defproc[(treelist-splitf-right [tl treelist?] [pred (-> any/c any/c)])
         treelist?]
@defproc[(treelist-prefix? [pre treelist?]
                           [tl treelist?]
                           [eql? (-> any/c any/c any/c) equal?])
         treelist?]
@defproc[(treelist-take-common-prefix [l treelist?]
                                      [r treelist?]
                                      [eql? (-> any/c any/c any/c) equal?])
         treelist?]
@defproc[(treelist-drop-common-prefix [l treelist?]
                                      [r treelist?]
                                      [eql? (-> any/c any/c any/c) equal?])
         (values treelist? treelist?)]
@defproc[(treelist-split-common-prefix [l treelist?]
                                       [r treelist?]
                                       [eql? (-> any/c any/c any/c) equal?])
         (values treelist? treelist? treelist?)]
@defproc[(treelist-add-between [tl treelist?]
                               [v any/c]
                               [#:before-first before-first treelist? empty-treelist]
                               [#:before-last before-last any/c v]
                               [#:after-last after-last treelist? empty-treelist]
                               [#:splice? splice? #false])
         treelist?]
@defproc[(treelist-check-duplicates
          [tl treelist?]
          [eql? (-> any/c any/c any/c) equal?]
          [#:key extract-key (λ (x) x)]
          [#:default failure-result failure-result/c (λ () #false)])
         any/c]
@defproc[(treelist-remove-duplicates [tl treelist?]
                                     [eql? (-> any/c any/c any/c) equal?]
                                     [#:key extract-key (λ (x) x)])
         treelist?]
@defproc[(treelist-filter-map [proc procedure?] [tl treelist?] ...+) treelist?]
@defproc[(treelist-count [proc procedure?] [tl treelist?] ...+) treelist?]
@defproc[(treelist-partition [pred (-> any/c any/c)] [tl treelist?])
         (values treelist? treelist?)]
@defproc*[([(treelist-range [end real?]) treelist?]
           [(treelist-range [start real?] [end real?] [step real? 1])
            treelist?])]
@defproc[(treelist-inclusive-range [start real?] [end real?] [step real? 1])
         treelist?]
@defproc[(treelist-append-map [proc procedure?] [tl treelist?] ...+) treelist?]
@defproc[(treelist-filter-not [pred (-> any/c any/c)] [tl treelist?]) treelist?]
@defproc[(treelist-argmin [proc (-> any/c real?)] [tl treelist?]) any/c]
@defproc[(treelist-argmax [proc (-> any/c real?)] [tl treelist?]) any/c]
@defproc[(treelist-group-by [key (-> any/c any/c)]
                            [tl treelist?]
                            [eql? (-> any/c any/c any/c) equal?])
         treelist?]]]{
Operations similar to their counterparts from @racketmodname[racket/list].
}

@deftogether[[
@defproc[(treelist-cartesian-product [tl treelist?] ...) treelist?]
@defproc[(treelist-cartesian-product* [tls treelist?]) treelist?]]]{
Computes the n-ary cartesian product of the given treelists.

@examples[
#:eval the-eval
(treelist-cartesian-product (treelist 1 2 3) (treelist 'a 'b 'c))
(treelist-cartesian-product*
 (treelist (treelist 1 2 3) (treelist 'a 'b 'c)))
(treelist-cartesian-product (treelist 4 5 6) (treelist 'd 'e 'f) (treelist #t #f))
(treelist-cartesian-product*
 (treelist (treelist 4 5 6) (treelist 'd 'e 'f) (treelist #t #f)))
]}
