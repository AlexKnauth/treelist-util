#lang rhombus/scribble/manual

@(import:
    rhombus meta_label: open
    treelist_util meta_label: open)
@(def the_eval: make_rhombus_eval())
@examples(
  ~eval: the_eval,
  ~hidden:
    import: treelist_util open
    fun is_positive(x :: Real) :: Boolean: 0 < x
    fun is_negative(x :: Real) :: Boolean: x < 0
    fun is_even(x :: Integral) :: Boolean: (x mod 2) == 0
    fun is_odd(x :: Integral) :: Boolean: (x mod 2) == 1
)

@title{treelist_util}

@//author{Alex Knauth}

@docmodule(treelist_util)

List operations matching @racketmodname(racket/list) that aren't in
@rhombus(rhombus.List, ~annot).

@doc(
  fun List.index_of(l :: List, v :: Any) :: maybe(NonnegInt)
){
Returns the index of the first element in @rhombus(l) that
is @rhombus(==) to @rhombus(v).
If no such element is found, the result is @rhombus(#false).

@examples(
  ~eval: the_eval,
  ~check:
    List.index_of([2, 4, 5, 8], 5)
    ~is 2
  ~check:
    List.index_of([2, 4, 5, 8], 6)
    ~is #false
)
}

@doc(
  fun List.index_where(l :: List, pred :: Function.of_arity(1)) :: maybe(NonnegInt)
){
Returns the index of the first element in @rhombus(l) where
applying @rhombus(pred) to the element produces a true value.
If no such element is found, the result is @rhombus(#false).

@examples(
  ~eval: the_eval,
  ~check:
    List.index_where([2, 4, 5, 8], is_odd)
    ~is 2
  ~check:
    List.index_where([2, 4, 5, 8], is_negative)
    ~is #false
)
}

@doc(
  fun List.splitf(l :: List, pred :: Function.of_arity(1)) :: values(List, List)
){
Splits @rhombus(l) into 2 lists returned as values.
The first list contains elements taken successivly from
@rhombus(l) as long as they satisfy @rhombus(pred).
The second list the rest of the elements of @rhombus(l),
from the first element not satisfying @rhombus(pred) and
onward.

@examples(
  ~eval: the_eval,
  ~check:
    List.splitf([2, 4, 5, 8], is_even)
    ~is values([2, 4], [5, 8])
)
}

@doc(
  fun List.cartesian_product(l :: List, ...) :: List.of(List),
  fun List.cartesian_product_all(ls :: List.of(List)) :: List.of(List)
){
Computes the n-ary cartesian product of the given lists.

@examples(
  ~eval: the_eval,
  ~check:
    List.cartesian_product([1, 2, 3], ["a", "b", "c"])
    ~is [[1, "a"],
         [1, "b"],
         [1, "c"],
         [2, "a"],
         [2, "b"],
         [2, "c"],
         [3, "a"],
         [3, "b"],
         [3, "c"]],
  ~check:
    List.cartesian_product_all([[4, 5, 6], ["d", "e", "f"], [#true, #false]])
    ~is [[4, "d", #true],
         [4, "d", #false],
         [4, "e", #true],
         [4, "e", #false],
         [4, "f", #true],
         [4, "f", #false],
         [5, "d", #true],
         [5, "d", #false],
         [5, "e", #true],
         [5, "e", #false],
         [5, "f", #true],
         [5, "f", #false],
         [6, "d", #true],
         [6, "d", #false],
         [6, "e", #true],
         [6, "e", #false],
         [6, "f", #true],
         [6, "f", #false]]
)
}
