#lang racket/base

(provide treelist*
         build-treelist
         treelist-andmap
         treelist-ormap
         treelist-foldl
         treelist-foldr
         treelist-remove
         treelist-remq
         treelist-remv
         treelist-remw
         treelist-remf
         treelist-remove*
         treelist-remq*
         treelist-remv*
         treelist-remw*
         treelist-second
         treelist-third
         treelist-fourth
         treelist-fifth
         treelist-sixth
         treelist-seventh
         treelist-eighth
         treelist-ninth
         treelist-tenth
         treelist-eleventh
         treelist-twelfth
         treelist-thirteenth
         treelist-fourteenth
         treelist-fifteenth
         treelist-update
         treelist-index-where
         treelist-indexes-of
         treelist-indexes-where
         treelist-split
         treelist-takef
         treelist-dropf
         treelist-splitf
         treelist-take-right
         treelist-drop-right
         treelist-split-right
         treelist-takef-right
         treelist-dropf-right
         treelist-splitf-right
         treelist-prefix?
         treelist-take-common-prefix
         treelist-drop-common-prefix
         treelist-split-common-prefix
         treelist-add-between
         treelist-check-duplicates
         treelist-remove-duplicates
         treelist-filter-map
         treelist-count
         treelist-partition
         treelist-range
         treelist-inclusive-range
         treelist-append-map
         treelist-filter-not
         treelist-argmin
         treelist-argmax
         treelist-group-by
         treelist-cartesian-product
         treelist-cartesian-product*)

(require racket/list
         racket/treelist
         treelist-util/util/condcomp)
(module+ test
  (require rackunit)
  (define-syntax-rule (vs->l expr)
    (call-with-values (λ () expr) list))
  (define-syntax-rule (check-values actual expected)
    (check-equal? (vs->l actual) (vs->l expected))))

(define (check-treelist who tl)
  (unless (treelist? tl)
    (raise-argument-error* who 'racket/primitive "treelist?" tl)))

(define (check-treelist-index who tl size index)
  (check-range* who 'racket/primitive "treelist" "" index tl 0 (sub1 size)))

(define (check-treelist-end-index who tl size index)
  (check-range* who 'racket/primitive "treelist" "" index tl 0 size))

(define (check-range* name realm desc pre index val lower upper)
  (unless (exact-nonnegative-integer? index)
    (raise-argument-error* name realm "exact-nonnegative-integer?" index))
  (unless (<= lower index upper)
    (raise-range-error* name realm desc pre index val lower upper)))

(define (check-procedure-arity-includes who proc n)
  (unless (and (procedure? proc) (procedure-arity-includes? proc n))
    (raise-argument-error* who
                           'racket/primitive
                           (format "(procedure-arity-includes/c ~v)" n)
                           proc)))

(define (map-reverse f as)
  (for/fold ([rbs '()]) ([a (in-list as)]) (cons (f a) rbs)))

(module+ test
  (check-equal? (map-reverse symbol->string '(a b c)) '("c" "b" "a")))

;; ---------------------------------------------------------

(define (treelist* a . bsc)
  (define-values [abs cs] (split-at-right (cons a bsc) 1))
  (define c (first cs))
  (check-treelist 'treelist* c)
  (treelist-append (list->treelist abs) c))

(module+ test
  (check-equal? (treelist* (treelist 'a 'b 'c 'd))
                (treelist 'a 'b 'c 'd))
  (check-equal? (treelist* 'a 'b 'c 'd (treelist 'e 'f 'g))
                (treelist 'a 'b 'c 'd 'e 'f 'g)))

(define (build-treelist n proc)
  (vector->treelist (build-vector n proc)))

(module+ test
  (check-equal? (build-treelist 10 values)
                (treelist 0 1 2 3 4 5 6 7 8 9))
  (check-equal? (build-treelist 5 (λ (x) (* x x)))
                (treelist 0 1 4 9 16)))

;; treelist-tail would be an alias of treelist-drop

(define (treelist-andmap proc as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-andmap proc m)
  (check-treelist 'treelist-andmap as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-andmap bs))
  (for/and ([a (in-treelist as)] [i (in-naturals)])
    (apply proc a (for/list ([bs (in-list bss)]) (treelist-ref bs i)))))

(module+ test
  (check-equal? (treelist-andmap positive? (treelist 1 2 3)) #t)
  (check-exn #rx"positive\\?: contract violation\n *expected: real\\?\n *given: 'a"
             (λ () (treelist-andmap positive? (treelist 1 2 'a))))
  (check-equal? (treelist-andmap positive? (treelist 1 -2 'a)) #f)
  (check-equal? (treelist-andmap + (treelist 1 2 3) (treelist 4 5 6)) 9))

(define (treelist-ormap proc as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-ormap proc m)
  (check-treelist 'treelist-ormap as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-ormap bs))
  (for/or ([a (in-treelist as)] [i (in-naturals)])
    (apply proc a (for/list ([bs (in-list bss)]) (treelist-ref bs i)))))

(module+ test
  (check-equal? (treelist-ormap eq? (treelist 'a 'b 'c) (treelist 'a 'b 'c)) #t)
  (check-equal? (treelist-ormap positive? (treelist 1 2 'a)) #t)
  (check-equal? (treelist-ormap + (treelist 1 2 3) (treelist 4 5 6)) 5))

(define (treelist-foldl proc init as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-foldl proc (add1 m))
  (check-treelist 'treelist-foldl as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-foldl bs))
  (for/fold ([acc init])
            ([a (in-treelist as)] [i (in-naturals)])
    (apply proc a
           (reverse (cons acc (map-reverse (λ (bs) (treelist-ref bs i)) bss))))))

(module+ test
  (check-equal? (treelist-foldl cons '() (treelist 1 2 3 4))
                '(4 3 2 1))
  (check-equal? (treelist-foldl treelist* empty-treelist (treelist 1 2 3 4))
                (treelist 4 3 2 1))
  (check-equal? (treelist-foldl + 0 (treelist 1 2 3 4))
                10)
  (check-equal? (treelist-foldl (λ (a b result)
                                  (* result (- a b)))
                                1
                                (treelist 1 2 3)
                                (treelist 4 5 6))

                -27))

(define (treelist-foldr proc init as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-foldr proc (add1 m))
  (check-treelist 'treelist-foldr as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-foldr bs))
  (for/foldr ([acc init])
             ([a (in-treelist as)] [i (in-naturals)])
    (apply proc a
           (reverse (cons acc (map-reverse (λ (bs) (treelist-ref bs i)) bss))))))

(module+ test
  (check-equal? (treelist-foldr cons '() (treelist 1 2 3 4))
                '(1 2 3 4))
  (check-equal? (treelist-foldr treelist* empty-treelist (treelist 1 2 3 4))
                (treelist 1 2 3 4))
  (check-equal? (treelist-foldr (λ (v l) (cons (add1 v) l)) '() (treelist 1 2 3 4))
                '(2 3 4 5))
  (check-equal? (treelist-foldr (λ (v l) (treelist* (add1 v) l))
                                empty-treelist
                                (treelist 1 2 3 4))
                (treelist 2 3 4 5)))

(if-not-exported
 racket/treelist
 treelist-filter
 (define (treelist-filter keep tl)
   (for/treelist ([el (in-treelist tl)] #:when (keep el)) el)))

(define (treelist-remove v tl [eql? equal?])
  (check-treelist 'treelist-remove tl)
  (check-procedure-arity-includes 'treelist-remove eql? 2)
  (define i (treelist-index-of tl v eql?))
  (cond
    [i (treelist-delete tl i)]
    [else tl]))

(module+ test
  (check-equal? (treelist-remove 2 (treelist 1 2 3 2 4))
                (treelist 1 3 2 4))
  (check-equal? (treelist-remove '(2) (treelist '(1) '(2) '(3)))
                (treelist '(1) '(3)))
  (check-equal? (treelist-remove (treelist 2)
                                 (treelist (treelist 1) (treelist 2) (treelist 3)))
                (treelist (treelist 1) (treelist 3)))
  (check-equal? (treelist-remove "2" (treelist "1" "2" "3"))
                (treelist "1" "3"))
  (check-equal? (treelist-remove #\c (treelist #\a #\b #\c))
                (treelist #\a #\b))
  (check-equal? (treelist-remove "B" (treelist "a" "A" "b" "B") string-ci=?)
                (treelist "a" "A" "B"))
  (check-equal? (treelist-remove 5 (treelist 1 2 3 2 4))
                (treelist 1 2 3 2 4))
  (let ([tl (treelist 1 2 3 2 4)])
    (check-eq? (treelist-remove 5 tl) tl)))

(define (treelist-remq v tl) (treelist-remove v tl eq?))
(define (treelist-remv v tl) (treelist-remove v tl eqv?))
(define (treelist-remw v tl) (treelist-remove v tl equal-always?))

(define (treelist-remf pred tl)
  (check-procedure-arity-includes 'treelist-remf pred 1)
  (check-treelist 'treelist-remf tl)
  (define i (treelist-index-where tl pred))
  (cond
    [i (treelist-delete tl i)]
    [else tl]))

(module+ test
  (check-equal? (treelist-remf negative? (treelist 1 -2 3 4 -5))
                (treelist 1 3 4 -5)))

(define (treelist-remove* vs tl [eql? equal?])
  (check-treelist 'treelist-remove* vs)
  (check-treelist 'treelist-remove* tl)
  (check-procedure-arity-includes 'treelist-remove* eql? 2)
  (treelist-filter (λ (a) (not (treelist-member? vs a eql?))) tl))

(module+ test
  (check-equal? (treelist-remove* (treelist 1 2) (treelist 1 2 3 2 4 5 2))
                (treelist 3 4 5)))

(define (treelist-remq* vs tl) (treelist-remove* vs tl eq?))
(define (treelist-remv* vs tl) (treelist-remove* vs tl eqv?))
(define (treelist-remw* vs tl) (treelist-remove* vs tl equal-always?))

;; remf* would be an alias for treelist-filter-not

;; ---------------------------------------------------------

(define (treelist-second tl) (treelist-ref tl 1))
(define (treelist-third tl) (treelist-ref tl 2))
(define (treelist-fourth tl) (treelist-ref tl 3))
(define (treelist-fifth tl) (treelist-ref tl 4))
(define (treelist-sixth tl) (treelist-ref tl 5))
(define (treelist-seventh tl) (treelist-ref tl 6))
(define (treelist-eighth tl) (treelist-ref tl 7))
(define (treelist-ninth tl) (treelist-ref tl 8))
(define (treelist-tenth tl) (treelist-ref tl 9))
(define (treelist-eleventh tl) (treelist-ref tl 10))
(define (treelist-twelfth tl) (treelist-ref tl 11))
(define (treelist-thirteenth tl) (treelist-ref tl 12))
(define (treelist-fourteenth tl) (treelist-ref tl 13))
(define (treelist-fifteenth tl) (treelist-ref tl 14))

(define (treelist-update tl pos updater)
  (check-treelist 'treelist-update tl)
  (check-treelist-index 'treelist-update tl (treelist-length tl) pos)
  (check-procedure-arity-includes 'treelist-update updater 1)
  (treelist-set tl pos (updater (treelist-ref tl pos))))

(module+ test
  (check-equal? (treelist-update (treelist 'zero 'one 'two) 1 symbol->string)
                (treelist 'zero "one" 'two)))

(if-not-exported
 racket/treelist
 treelist-index-of
 (define (treelist-index-of tl v [eql? equal?])
   (for/first ([el (in-treelist tl)]
               [i (in-naturals)]
               #:when (eql? el v))
     i)))

(define (treelist-index-where tl match?)
  (check-treelist 'treelist-index-where tl)
  (check-procedure-arity-includes 'treelist-index-where match? 1)
  (for/first ([el (in-treelist tl)]
              [i (in-naturals)]
              #:when (match? el))
    i))

(module+ test
  (define small-treelist (treelist 0 "a" 'b '#:c))

  (check-equal? (treelist-index-where small-treelist number?) 0)
  (check-equal? (treelist-index-where small-treelist string?) 1)
  (check-equal? (treelist-index-where small-treelist symbol?) 2)
  (check-equal? (treelist-index-where small-treelist keyword?) 3)
  (check-equal? (treelist-index-where small-treelist void?) #f))

(define (treelist-indexes-of tl v [eql? equal?])
  (check-treelist 'treelist-indexes-of tl)
  (check-procedure-arity-includes 'treelist-indexes-of eql? 2)
  (for/treelist ([el (in-treelist tl)]
                 [i (in-naturals)]
                 #:when (eql? el v))
    i))

(module+ test
  (check-equal? (treelist-indexes-of (treelist 1 2 1 2 1) 2)
                (treelist 1 3)))

(define (treelist-indexes-where tl match?)
  (check-treelist 'treelist-indexes-where tl)
  (check-procedure-arity-includes 'treelist-indexes-where match? 1)
  (for/treelist ([el (in-treelist tl)]
                 [i (in-naturals)]
                 #:when (match? el))
    i))

(module+ test
  (check-equal? (treelist-indexes-where (treelist 1 2 3 4) even?)
                (treelist 1 3)))

(define (treelist-split tl at)
  (check-treelist 'treelist-split tl)
  (define n (treelist-length tl))
  (check-treelist-end-index 'treelist-split tl n at)
  (cond
    [(zero? at) (values empty-treelist tl)]
    [(= at n) (values tl empty-treelist)]
    [else (values (treelist-take tl at) (treelist-drop tl at))]))

(module+ test
  (check-values (treelist-split small-treelist 0)
                (values empty-treelist small-treelist))
  (check-values (treelist-split small-treelist 1)
                (values (treelist 0) (treelist "a" 'b '#:c)))
  (check-values (treelist-split small-treelist 2)
                (values (treelist 0 "a") (treelist 'b '#:c)))
  (check-values (treelist-split small-treelist 3)
                (values (treelist 0 "a" 'b) (treelist '#:c)))
  (check-values (treelist-split small-treelist 4)
                (values small-treelist empty-treelist)))

(define (treelist-takef tl match?)
  (check-treelist 'treelist-takef tl)
  (check-procedure-arity-includes 'treelist-takef match? 1)
  (define at (treelist-index-where tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-take tl at)]
    [else tl]))

(module+ test
  (check-equal? (treelist-takef small-treelist number?)
                (treelist 0))
  (check-equal? (treelist-takef small-treelist string?)
                empty-treelist)
  (check-equal? (treelist-takef (treelist 2 4 5 8) even?)
                (treelist 2 4))
  (check-equal? (treelist-takef (treelist 2 4 5 8) odd?)
                empty-treelist)
  (check-equal? (treelist-takef (treelist 2 4 6 8) even?)
                (treelist 2 4 6 8))
  (check-equal? (treelist-takef (treelist 2 4 6 8) odd?)
                empty-treelist))

(define (treelist-dropf tl match?)
  (check-treelist 'treelist-dropf tl)
  (check-procedure-arity-includes 'treelist-dropf match? 1)
  (define at (treelist-index-where tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-drop tl at)]
    [else empty-treelist]))

(module+ test
  (check-equal? (treelist-dropf small-treelist number?)
                (treelist "a" 'b '#:c))
  (check-equal? (treelist-dropf small-treelist string?)
                small-treelist)
  (check-equal? (treelist-dropf (treelist 2 4 5 8) even?)
                (treelist 5 8))
  (check-equal? (treelist-dropf (treelist 2 4 5 8) odd?)
                (treelist 2 4 5 8))
  (check-equal? (treelist-dropf (treelist 2 4 6 8) even?)
                empty-treelist)
  (check-equal? (treelist-dropf (treelist 2 4 6 8) odd?)
                (treelist 2 4 6 8)))

(define (treelist-splitf tl match?)
  (check-treelist 'treelist-splitf tl)
  (check-procedure-arity-includes 'treelist-splitf match? 1)
  (define at (treelist-index-where tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-split tl at)]
    [else (values tl empty-treelist)]))

(module+ test
  (check-values (treelist-splitf small-treelist number?)
                (values (treelist 0) (treelist "a" 'b '#:c)))
  (check-values (treelist-splitf small-treelist string?)
                (values empty-treelist small-treelist))
  (check-values (treelist-splitf (treelist 2 4 5 8) even?)
                (values (treelist 2 4) (treelist 5 8)))
  (check-values (treelist-splitf (treelist 2 4 5 8) odd?)
                (values empty-treelist (treelist 2 4 5 8)))
  (check-values (treelist-splitf (treelist 2 4 6 8) even?)
                (values (treelist 2 4 6 8) empty-treelist))
  (check-values (treelist-splitf (treelist 2 4 6 8) odd?)
                (values empty-treelist (treelist 2 4 6 8))))

(define (treelist-take-right tl pos)
  (define n (treelist-length tl))
  (treelist-drop tl (- n pos)))

(module+ test
  (check-equal? (treelist-take-right (treelist 1 2 3 4 5) 2)
                (treelist 4 5)))

(define (treelist-drop-right tl pos)
  (define n (treelist-length tl))
  (treelist-take tl (- n pos)))

(module+ test
  (check-equal? (treelist-drop-right (treelist 1 2 3 4 5) 2)
                (treelist 1 2 3)))

(define (treelist-split-right tl pos)
  (define n (treelist-length tl))
  (treelist-split tl (- n pos)))

(module+ test
  (check-values (treelist-split-right (treelist 1 2 3 4 5 6) 4)
                (values (treelist 1 2) (treelist 3 4 5 6))))

(define (treelist-index-where-right tl match?)
  (define n (treelist-length tl))
  (for/first ([i (in-range (sub1 n) -1 -1)]
              #:when (match? (treelist-ref tl i)))
    i))

(module+ test
  (check-equal? (treelist-index-where-right (treelist 0 1 2 3 4 5 6 7 8 9) even?)
                8)
  (check-equal? (treelist-index-where-right (treelist 0 1 2 3 4 5 6 7 8 9) odd?)
                9)
  (check-equal? (treelist-index-where-right (treelist 0 1 2 3 4 5 6 7 8 10) odd?)
                7)
  (check-equal? (treelist-index-where-right (treelist 0 1 2 3 4 5 6 6 8 10) odd?)
                5)
  (check-equal? (treelist-index-where-right (treelist 0 1 2 3 4 6 6 8 10) odd?)
                3)
  (check-equal? (treelist-index-where-right (treelist 0 1 2 2 4 6 6 8 10) odd?)
                1)
  (check-equal? (treelist-index-where-right (treelist 0 2 2 2 4 6 6 8 10) odd?)
                #false))

(define (treelist-takef-right tl match?)
  (check-treelist 'treelist-takef-right tl)
  (check-procedure-arity-includes 'treelist-takef-right match? 1)
  (define at (treelist-index-where-right tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-drop tl (add1 at))]
    [else tl]))

(module+ test
  (check-equal? (treelist-takef-right (treelist 'a "b" 'c "d" "e" "f" "g") symbol?)
                empty-treelist)
  (check-equal? (treelist-takef-right (treelist 'a "b" 'c "d" 'e 'f 'g) symbol?)
                (treelist 'e 'f 'g))
  (check-equal? (treelist-takef-right (treelist 'a 'b 'c 'd 'e 'f 'g) symbol?)
                (treelist 'a 'b 'c 'd 'e 'f 'g)))

(define (treelist-dropf-right tl match?)
  (check-treelist 'treelist-dropf-right tl)
  (check-procedure-arity-includes 'treelist-dropf-right match? 1)
  (define at (treelist-index-where-right tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-take tl (add1 at))]
    [else empty-treelist]))

(module+ test
  (check-equal? (treelist-dropf-right (treelist 'a "b" 'c "d" "e" "f" "g") symbol?)
                (treelist 'a "b" 'c "d" "e" "f" "g"))
  (check-equal? (treelist-dropf-right (treelist 'a "b" 'c "d" 'e 'f 'g) symbol?)
                (treelist 'a "b" 'c "d"))
  (check-equal? (treelist-dropf-right (treelist 'a 'b 'c 'd 'e 'f 'g) symbol?)
                empty-treelist))

(define (treelist-splitf-right tl match?)
  (check-treelist 'treelist-splitf-right tl)
  (check-procedure-arity-includes 'treelist-splitf-right match? 1)
  (define at (treelist-index-where-right tl (λ (el) (not (match? el)))))
  (cond
    [at (treelist-split tl (add1 at))]
    [else (values empty-treelist tl)]))

(module+ test
  (check-values (treelist-splitf-right (treelist 'a "b" 'c "d" "e" "f" "g")
                                       symbol?)
                (values (treelist 'a "b" 'c "d" "e" "f" "g")
                        empty-treelist))
  (check-values (treelist-splitf-right (treelist 'a "b" 'c "d" 'e 'f 'g)
                                       symbol?)
                (values (treelist 'a "b" 'c "d")
                        (treelist 'e 'f 'g)))
  (check-values (treelist-splitf-right (treelist 'a 'b 'c 'd 'e 'f 'g)
                                       symbol?)
                (values empty-treelist
                        (treelist 'a 'b 'c 'd 'e 'f 'g))))

(define (treelist-prefix? pre tl [eql? equal?])
  (check-treelist 'treelist-prefix? pre)
  (check-treelist 'treelist-prefix? tl)
  (check-procedure-arity-includes 'treelist-prefix? eql? 2)
  (and (<= (treelist-length pre) (treelist-length tl))
       (for/and ([ep (in-treelist pre)] [el (in-treelist tl)])
         (eql? ep el))))

(module+ test
  (check-equal? (treelist-prefix? (treelist 1 2) (treelist 1 2 3 4 5)) #true)
  (check-equal? (treelist-prefix? (treelist 1 3) (treelist 1 2 3 4 5)) #false)
  (check-equal? (treelist-prefix? (treelist 1 2) (treelist 1)) #false))

(define (treelist-take-common-prefix l r [eql? equal?])
  (check-treelist 'treelist-take-common-prefix l)
  (check-treelist 'treelist-take-common-prefix r)
  (check-procedure-arity-includes 'treelist-take-common-prefix eql? 2)
  (for/treelist ([el (in-treelist l)]
                 [er (in-treelist r)]
                 #:break (not (eql? el er)))
    el))

(module+ test
  (check-equal? (treelist-take-common-prefix (treelist 'a 'b 'c 'd)
                                             (treelist 'a 'b 'x 'y 'z))
                (treelist 'a 'b)))

(define (treelist-common-prefix-length l r [eql? equal?])
  (for/fold ([acc 0])
            ([el (in-treelist l)]
             [er (in-treelist r)]
             [i (in-naturals 1)]
             #:break (not (eql? el er)))
    i))

(module+ test
  (check-equal? (treelist-common-prefix-length (treelist 'a 'b 'c 'd)
                                               (treelist 'a 'b 'x 'y 'z))
                2))

(define (treelist-drop-common-prefix l r [eql? equal?])
  (check-treelist 'treelist-take-common-prefix l)
  (check-treelist 'treelist-take-common-prefix r)
  (check-procedure-arity-includes 'treelist-take-common-prefix eql? 2)
  (define i (treelist-common-prefix-length l r eql?))
  (values (treelist-drop l i)
          (treelist-drop r i)))

(module+ test
  (check-values (treelist-drop-common-prefix (treelist 'a 'b 'c 'd)
                                             (treelist 'a 'b 'x 'y 'z))
                (values (treelist 'c 'd)
                        (treelist 'x 'y 'z))))

(define (treelist-split-common-prefix l r [eql? equal?])
  (check-treelist 'treelist-take-common-prefix l)
  (check-treelist 'treelist-take-common-prefix r)
  (check-procedure-arity-includes 'treelist-take-common-prefix eql? 2)
  (define pre (treelist-take-common-prefix l r eql?))
  (define i (treelist-length pre))
  (values pre
          (treelist-drop l i)
          (treelist-drop r i)))

(module+ test
  (check-values (treelist-split-common-prefix (treelist 'a 'b 'c 'd)
                                              (treelist 'a 'b 'x 'y 'z))
                (values (treelist 'a 'b)
                        (treelist 'c 'd)
                        (treelist 'x 'y 'z))))

;; treelist-add-between
(module+ test
  (check-equal? (treelist-add-between (treelist 'x 'y 'z) 'and)
                (treelist 'x 'and 'y 'and 'z))
  (check-equal? (treelist-add-between (treelist 'x) 'and)
                (treelist 'x))
  (check-equal? (treelist-add-between (treelist "a" "b" "c" "d")
                                      ","
                                      #:before-last "and")
                (treelist "a" "," "b" "," "c" "and" "d"))
  (check-equal? (treelist-add-between (treelist 'x 'y 'z)
                                      (treelist '-)
                                      #:before-first (treelist 'begin)
                                      #:before-last (treelist '- '-)
                                      #:after-last (treelist 'end 'LF)
                                      #:splice? #t)
                (treelist 'begin 'x '- 'y '- '- 'z 'end 'LF)))

(define (treelist-add-between tl
                              v
                              #:before-first [before-first empty-treelist]
                              #:before-last [before-last v]
                              #:after-last [after-last empty-treelist]
                              #:splice? [splice? #false])
  (define vs (if splice? v (treelist v)))
  (define before-lasts (if splice? before-last (treelist before-last)))
  (define nm1 (sub1 (treelist-length tl)))
  (treelist-append
   (for/fold ([acc before-first])
             ([el (in-treelist tl)]
              [i (in-naturals)])
     (cond
       [(zero? i) (treelist-add acc el)]
       [(= i nm1) (treelist-add (treelist-append acc before-lasts) el)]
       [else (treelist-add (treelist-append acc vs) el)]))
   after-last))

;; treelist-check-duplicates
(module+ test
  (check-equal? (treelist-check-duplicates (treelist 1 2 3 4))
                #f)
  (check-equal? (treelist-check-duplicates (treelist 1 2 3 2 1))
                2)
  (check-equal? (treelist-check-duplicates (treelist '(a 1) '(b 2) '(a 3)) #:key car)
                '(a 3))
  (check-equal? (treelist-check-duplicates (treelist (treelist 'a 1)
                                                     (treelist 'b 2)
                                                     (treelist 'a 3))
                                           #:key treelist-first)
                (treelist 'a 3))
  (check-equal? (treelist-check-duplicates (treelist 1 2 3 4 5 6)
                                           (λ (x y) (= (modulo x 3) (modulo y 3))))
                4)
  (check-equal? (treelist-check-duplicates (treelist 1 2 3 4)
                                           #:default "no duplicates")
                "no duplicates"))

(define (treelist-check-duplicates tl
                                   [eql? equal?]
                                   #:key [extract-key values]
                                   #:default [failure-result (λ () #f)])
  (define fail (if (procedure? failure-result) failure-result (λ () failure-result)))
  (define n (treelist-length tl))
  (let loop ([seen-keys empty-treelist] [i 0])
    (cond
      [(= i n) (fail)]
      [else
       (define el (treelist-ref tl i))
       (define ek (extract-key el))
       (cond
         [(treelist-member? seen-keys ek eql?) el]
         [else (loop (treelist-add seen-keys ek) (add1 i))])])))

;; treelist-remove-duplicates
(module+ test
  (check-equal? (treelist-remove-duplicates (treelist 'a 'b 'b 'a))
                (treelist 'a 'b))
  (check-equal? (treelist-remove-duplicates (treelist 1 2 1.0 0))
                (treelist 1 2 1.0 0))
  (check-equal? (treelist-remove-duplicates (treelist 1 2 1.0 0) =)
                (treelist 1 2 0)))

(define (treelist-remove-duplicates tl [eql? equal?] #:key [extract-key values])
  (define n (treelist-length tl))
  (let loop ([acc empty-treelist] [seen-keys empty-treelist] [i 0])
    (cond
      [(= i n) acc]
      [else
       (define el (treelist-ref tl i))
       (define ek (extract-key el))
       (cond
         [(treelist-member? seen-keys ek eql?)
          (loop acc seen-keys (add1 i))]
         [else
          (loop (treelist-add acc el) (treelist-add seen-keys ek) (add1 i))])])))

(define (treelist-filter-map proc as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-filter-map proc m)
  (check-treelist 'treelist-filter-map as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-filter-map bs))
  (for/treelist ([a (in-treelist as)]
                 [i (in-naturals)]
                 #:do [(define v
                         (apply proc
                                a
                                (for/list ([bs (in-list bss)])
                                  (treelist-ref bs i))))]
                 #:when v)
    v))

(module+ test
  (check-equal? (treelist-filter-map (λ (x) (and (negative? x) (abs x)))
                                     (treelist 1 2 -3 -4 8))
                (treelist 3 4)))

(define (treelist-count proc as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-filter-map proc m)
  (check-treelist 'treelist-filter-map as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-filter-map bs))
  (for/sum ([a (in-treelist as)]
            [i (in-naturals)]
            #:when (apply proc
                          a
                          (for/list ([bs (in-list bss)])
                            (treelist-ref bs i))))
    1))

(module+ test
  (check-equal? (treelist-count positive? (treelist 1 -1 2 3 -2 5))
                4))

(define (treelist-partition keep tl)
  (for/fold ([kept empty-treelist] [skipt empty-treelist])
            ([el (in-treelist tl)])
    (cond
      [(keep el) (values (treelist-add kept el) skipt)]
      [else (values kept (treelist-add skipt el))])))

(module+ test
  (check-values (treelist-partition even? (treelist 1 2 3 4 5 6))
                (values (treelist 2 4 6)
                        (treelist 1 3 5))))

(define treelist-range
  (case-lambda
    [(end) (build-treelist end values)]
    [(start end) (build-treelist (- end start) (λ (i) (+ start i)))]
    [(start end step) (for/treelist ([el (in-range start end step)]) el)]))

(module+ test
  (check-equal? (treelist-range 10)
                (treelist 0 1 2 3 4 5 6 7 8 9))
  (check-equal? (treelist-range 10 20)
                (treelist 10 11 12 13 14 15 16 17 18 19))
  (check-equal? (treelist-range 20 40 2)
                (treelist 20 22 24 26 28 30 32 34 36 38))
  (check-equal? (treelist-range 20 10 -1)
                (treelist 20 19 18 17 16 15 14 13 12 11))
  (check-equal? (treelist-range 10 15 1.5)
                (treelist 10 11.5 13.0 14.5)))

(define (treelist-inclusive-range start end [step 1])
  (for/treelist ([el (in-inclusive-range start end step)]) el))

(module+ test
  (check-equal? (treelist-inclusive-range 10 20)
                (treelist 10 11 12 13 14 15 16 17 18 19 20))
  (check-equal? (treelist-inclusive-range 20 40 2)
                (treelist 20 22 24 26 28 30 32 34 36 38 40))
  (check-equal? (treelist-inclusive-range 20 10 -1)
                (treelist 20 19 18 17 16 15 14 13 12 11 10))
  (check-equal? (treelist-inclusive-range 10 15 1.5)
                (treelist 10 11.5 13.0 14.5)))

(define (treelist-append-map proc as . bss)
  (define m (add1 (length bss)))
  (check-procedure-arity-includes 'treelist-append-map proc m)
  (check-treelist 'treelist-append-map as)
  (define n (treelist-length as))
  (for ([bs (in-list bss)])
    (check-treelist 'treelist-append-map bs))
  (for/fold ([acc empty-treelist])
            ([a (in-treelist as)] [i (in-naturals)])
    (treelist-append
     acc
     (apply proc a (for/list ([bs (in-list bss)]) (treelist-ref bs i))))))

(module+ test
  (check-equal? (treelist-append-map vector->treelist (treelist '#(1) '#(2 3) '#(4)))
                (treelist 1 2 3 4)))

(define (treelist-filter-not skip tl)
  (treelist-filter (λ (el) (not (skip el))) tl))

(module+ test
  (check-equal? (treelist-filter-not even? (treelist 1 2 3 4 5 6))
                (treelist 1 3 5)))

(define (treelist-argmin proc tl)
  (check-procedure-arity-includes 'treelist-argmin proc 1)
  (check-treelist 'treelist-argmin tl)
  (when (treelist-empty? tl)
    (error 'treelist-argmin "expected a non-empty treelist, given: ~v" tl))
  (define el0 (treelist-first tl))
  (define ek0 (proc el0))
  (for/fold ([e el0] [k ek0] #:result e)
            ([el (in-treelist tl)]
             #:unless (eq? e el))
    (define ek (proc el))
    (cond
      [(< ek k) (values el ek)]
      [else (values e k)])))

(module+ test
  (check-equal? (treelist-argmin car (treelist '(3 pears) '(1 banana) '(2 apples)))
                '(1 banana))
  (check-equal? (treelist-argmin car (treelist '(1 banana) '(1 orange)))
                '(1 banana))
  (check-equal? (treelist-argmin treelist-first
                                 (treelist (treelist 3 'pears)
                                           (treelist 1 'banana)
                                           (treelist 2 'apples)))
                (treelist 1 'banana))
  (check-equal? (treelist-argmin treelist-first
                                 (treelist (treelist 1 'banana)
                                           (treelist 1 'orange)))
                (treelist 1 'banana)))

(define (treelist-argmax proc tl)
  (check-procedure-arity-includes 'treelist-argmax proc 1)
  (check-treelist 'treelist-argmax tl)
  (when (treelist-empty? tl)
    (error 'treelist-argmax "expected a non-empty treelist, given: ~v" tl))
  (define el0 (treelist-first tl))
  (define ek0 (proc el0))
  (for/fold ([e el0] [k ek0] #:result e)
            ([el (in-treelist tl)]
             #:unless (eq? e el))
    (define ek (proc el))
    (cond
      [(< k ek) (values el ek)]
      [else (values e k)])))

(module+ test
  (check-equal? (treelist-argmax car (treelist '(3 pears) '(1 banana) '(2 apples)))
                '(3 pears))
  (check-equal? (treelist-argmax car (treelist '(3 pears) '(3 oranges)))
                '(3 pears))
  (check-equal? (treelist-argmax treelist-first
                                 (treelist (treelist 3 'pears)
                                           (treelist 1 'banana)
                                           (treelist 2 'apples)))
                (treelist 3 'pears))
  (check-equal? (treelist-argmax treelist-first
                                 (treelist (treelist 3 'pears)
                                           (treelist 3 'oranges)))
                (treelist 3 'pears)))

(define (treelist-group-by key tl [eql? equal?])
  (check-procedure-arity-includes 'treelist-group-by key 1)
  (check-treelist 'treelist-group-by tl)
  (check-procedure-arity-includes 'treelist-group-by eql? 2)
  ; table : (Treelistof (cons K (Treelistof V)))
  (for/fold ([table empty-treelist] #:result (treelist-map table cdr))
            ([el (in-treelist tl)])
    (define ek (key el))
    (define i (treelist-index-where table (λ (kvs) (eql? (car kvs) ek))))
    (cond
      [i
       (define kvs (treelist-ref table i))
       (treelist-set table i (cons (car kvs) (treelist-add (cdr kvs) el)))]
      [else
       (treelist-add table (cons ek (treelist el)))])))

(module+ test
  (check-equal? (treelist-group-by (λ (x) (modulo x 3))
                                   (treelist 1 2 1 2 54 2 5 43 7 2 643 1 2 0))
                (treelist (treelist 1 1 43 7 643 1)
                          (treelist 2 2 2 5 2 2)
                          (treelist 54 0))))

;; treelist-cartesian-product, treelist-cartesian-product*
(module+ test
  (check-equal? (treelist-cartesian-product (treelist 1 2 3) (treelist 'a 'b 'c))
                (treelist (treelist 1 'a)
                          (treelist 1 'b)
                          (treelist 1 'c)
                          (treelist 2 'a)
                          (treelist 2 'b)
                          (treelist 2 'c)
                          (treelist 3 'a)
                          (treelist 3 'b)
                          (treelist 3 'c)))
  (check-equal? (treelist-cartesian-product*
                 (treelist (treelist 1 2 3) (treelist 'a 'b 'c)))
                (treelist (treelist 1 'a)
                          (treelist 1 'b)
                          (treelist 1 'c)
                          (treelist 2 'a)
                          (treelist 2 'b)
                          (treelist 2 'c)
                          (treelist 3 'a)
                          (treelist 3 'b)
                          (treelist 3 'c)))
  (check-equal? (treelist-cartesian-product (treelist 4 5 6)
                                            (treelist 'd 'e 'f)
                                            (treelist #t #f))
                (treelist (treelist 4 'd #t)
                          (treelist 4 'd #f)
                          (treelist 4 'e #t)
                          (treelist 4 'e #f)
                          (treelist 4 'f #t)
                          (treelist 4 'f #f)
                          (treelist 5 'd #t)
                          (treelist 5 'd #f)
                          (treelist 5 'e #t)
                          (treelist 5 'e #f)
                          (treelist 5 'f #t)
                          (treelist 5 'f #f)
                          (treelist 6 'd #t)
                          (treelist 6 'd #f)
                          (treelist 6 'e #t)
                          (treelist 6 'e #f)
                          (treelist 6 'f #t)
                          (treelist 6 'f #f)))
  (check-equal? (treelist-cartesian-product*
                 (treelist (treelist 4 5 6)
                           (treelist 'd 'e 'f)
                           (treelist #t #f)))
                (treelist (treelist 4 'd #t)
                          (treelist 4 'd #f)
                          (treelist 4 'e #t)
                          (treelist 4 'e #f)
                          (treelist 4 'f #t)
                          (treelist 4 'f #f)
                          (treelist 5 'd #t)
                          (treelist 5 'd #f)
                          (treelist 5 'e #t)
                          (treelist 5 'e #f)
                          (treelist 5 'f #t)
                          (treelist 5 'f #f)
                          (treelist 6 'd #t)
                          (treelist 6 'd #f)
                          (treelist 6 'e #t)
                          (treelist 6 'e #f)
                          (treelist 6 'f #t)
                          (treelist 6 'f #f))))

(define (treelist-cartesian-product-add ass bs)
  (for*/treelist ([as (in-treelist ass)] [b (in-treelist bs)])
    (treelist-add as b)))

(define (treelist-cartesian-product . tls)
  (for/fold ([acc (treelist empty-treelist)])
            ([tl (in-list tls)])
    (treelist-cartesian-product-add acc tl)))

(define (treelist-cartesian-product* tls)
  (for/fold ([acc (treelist empty-treelist)])
            ([tl (in-treelist tls)])
    (treelist-cartesian-product-add acc tl)))

