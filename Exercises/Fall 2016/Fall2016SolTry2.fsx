// Problem 1
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point

type Scoreboard = Score list

let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30); ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;

// Q1.1
let rec inv scoreb =
    match scoreb with
    | [] -> true
    | [(_,_,p)] -> p>=0
    | (_,_,p)::((_,_,p1)::sb as tail) when p>=p1-> inv tail
    | _ -> false

// Q1.2
let rec insert (ns,es,ps) sb =
    match sb with
    | [] -> [(ns,es,ps)]
    | (n,e,p)::sb when p>=ps -> (n,e,p)::insert (ns,es,ps) sb
    | (n,e,p)::sb -> (ns,es,ps)::(n,e,p)::sb

// Q1.3
let rec get (n,sb) =
    match sb with
    | [] -> []
    | (n1,e1,p1)::sb1 when n=n1 -> (e1,p1)::get (n,sb1)
    | (n1,e1,p1)::sb1 -> get (n,sb1)

// Q1.4
// let rec top k sb = 
//     match sb with
//     | [] -> None
//     | (n,e,p)::sb' when k>0 -> (n,e,p)::top (k-1) sb'
//     | (n,e,p)::sb' -> Some(sb')

// Problem 2
// Q2.1
let rec replace a b xs =
    match xs with
    | [] -> []
    | x::xs1 when x=a -> b::replace a b xs1
    | x::xs1 -> x::replace a b xs1

// Q2.2
// 'a -> 'a -> 'a list -> 'a list

// Q2.3


// Problem 3
// Q3.1
(*
    pos: seq<int> : increments by 1
    seq1: seq<Int*int> : tuple of i and of negative i
    val1: seq<int*int> : 5 elements from seq1, so 5 tuples
*)

// Q3.2
(*
    seq2: seq<int*int> : tuples of (i,0 to i)
    val2: list<int*int> : takes 10 elements from seq2 and adds them to a list
*)

// Problem 4

type Tree<'a,'b> = | A of 'a | B of 'b
                   | Node of Tree<'a,'b> * Tree<'a,'b>;;

// Q4.1
let v1 = A false;;
let v2 = B [1];;
let v3 = Node(v1,v2);;

// Q4.2
let rec countA t =
    match t with
    | A _ -> 1
    | B _ -> 0
    | Node(t1,t2) -> countA t1 + countA t2

// Q4.3


// Q4.4
(*
    G: Tree<'a,'b> -> Tree<'a,'b> : Swaps each branch with its corresponding branch
    F: Tree<'a,'b> -> a'list*b'list
*)

