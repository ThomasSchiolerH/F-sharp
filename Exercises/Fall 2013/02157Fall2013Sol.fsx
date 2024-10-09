// Problem 1

// Q1.1


// Problem 2
let rec f i = function
    | [] -> []
    | x::xs -> (i,x)::f (i*i) xs;;
// Q2.1
(*
    F: int -> 'a list -> (int * 'a list) : Recurses over each element and
    creates a tuple with the element and an integer i that is to the power of 2
    for each recursion.
    G: ('a -> bool) -> 'a tree -> Option<'a tree>
*)

// Q2.2


// Q2.3
(*
    h f (4, 4)
    h f (3, 12)
    h f (2, 24)
    h f (1, 24)
    h f (0, 24)
    prints 24

    ('a -> 'b) -> (int * int) -> int

    In the case of 4,1 it computes the factorial of 4
    But in other cases where e is not 1 it simple multiplies e
    with n!
*)

// Q2.4 
(*
    1. A: seq<int> B&C:seq<int*int>
    2. X: list<int> Y&Z: list<int*int>
*)


// Problem 3
type Title = string;;

type Section = Title * Elem list
and Elem = Par of string | Sub of Section;;

type Chapter = Title * Section list;;
type Book = Chapter list;;

// Q3.1
let maxL l =
    match l with
    | [] -> 0
    | _ -> List.max(l)

// Q3.2
let rec overview b =
    match b with
    | [] -> []
    | (t,_)::bs -> t::overview bs

// Q3.3


// Q3.4



