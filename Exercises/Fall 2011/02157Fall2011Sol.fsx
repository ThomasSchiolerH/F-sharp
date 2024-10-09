// Question 1

type name = string;;
type phone = int;;
type level = int;;

type description = phone * level;;
type register = (name * description) list;;

// Q1.1
let r = [("Joe",(10101010,4)), ("Sal",(11111111,2)), ("Sam",(12121212,7)),("Jane",(13131313,1))];;

// Q1.2
let rec getPhone name reg =
    match reg with
    | (n, (pn, _))::xs when n = name -> pn
    | (n, (pn,_))::xs -> getPhone name xs
    | _ -> failwith "Not a member"

// Q1.3
let rec delete (name,r) =
    match r with
    | (n, (p,nl))::xs when n = name -> xs
    | (n, (p,nl))::xs -> (n, (p,nl))::delete (name,xs)
    | [] -> failwith "Not a member"

// Q1.4
let rec getCandidates level r =
    match r with
    | [] -> []
    | (name, (pn, l))::xs when abs(l-level)<3 -> (name, pn)::getCandidates level xs
    | (name, (pn, l))::xs -> getCandidates level xs

// Problem 2

type exp = | C of int
           | BinOp of exp*string*exp
           | Id of string
           | Def of string * exp * exp;;

// Q2.1
let val1 = C 5;
let val2 = BinOp (C 3, "+", C 4)
let val3 =  BinOp (C 2, "*", BinOp (C 3, "+", C 4))

// Q2.2
let rec toString e =
    match e with
    | C num -> num.ToString()
    | BinOp(e1,op,e2) -> "("+e1.ToString()+op+e2.ToString()+")"

// Q2.3
let rec extractOp e =
    match e with
    | C _ -> []
    | BinOp(e1,op,e2) -> op::extractOp e1 @ extractOp e2

// Q2.4
//let rec isDef e : bool =


// Problem 3
// Q3.1
(*
    F: (int * a' tree) -> a' tree : computes / constructs a tree based on root value
    G: (a' -> bool) -> 'a tree -> 'a tree : checks the two branches for the predicate p,
    if it satisfies p then p is applied to it.
    H: ('a -> 'b) -> 'a tree -> 'b tree : computes a new tree that applies
    k to every alement of a.
*)

// Problem 4