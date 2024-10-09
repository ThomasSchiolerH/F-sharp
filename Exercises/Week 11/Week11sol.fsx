// Problem 1
// Q1.1
(*
    It is clear to see that the function takes a predicate as the first argument ('a->bool) seen from when p x
    xs is a list that is also takes as input and produces list as output

*)

// Q1.2
(*
    skipWhile diff5 [2;6;5;1;5;6]
=> skipWhile diff5 [6;5;1;5;6]
=> skipWhile diff5 [5;1;5;6]
=> [5;1;5;6].
*)

// Q1.3
// Done

// Q1.4
let rec takeWhile p acc = function
    | x::xs when p x -> takeWhile p xs (x::acc)
    | _ -> List.rev acc

// Problem 2
type T = Leaf of char | Branch of T*T

// Q2.1
let rec toList t =
    match t with
    | Leaf c -> [c]
    | Branch(t1,t2) -> toList t1 @ toList t2 

// Q2.2
let rec legal t =
    List.length (toList t) > 1 && toList t = List.distinct (toList t);;

// Q2.3
type Dir =  | L
            | R
type Code = Dir list
type CodingTable = Map<char, Code>

let rec encode codingtable charlist =
    match charlist with
    | [] -> []
    | c::rest -> Map.find c codingtable @ encode codingtable rest;;

