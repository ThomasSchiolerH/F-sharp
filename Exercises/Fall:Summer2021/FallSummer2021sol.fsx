// Problem 1

// Q1.1
(*
    
*)

// Problem 2
// Q2.1
(*
    F: ('a -> bool) -> T -> int list
*)

// Problem 3
type Trie<'a>    = N of 'a * bool * Children<'a>
and Children<'a> = Trie<'a> list

let t1 = N(0, false, [N(0, false, [N(1,true,[])])]);;
let t2 = N(0, true, [N(0, false, [N(1,true,[])])]);;
let ta = N(1,true,[N(2,true,[])]);;
let tb = N(3,false,[N(0,true,[])]);;
let tc = N(2,true,[]);;
let t3 = N(0,false, [ta;tb;tc]);;

// Q3.1
let rec countN t =
    match t with
    | N(a,bool,c) -> 1 + countN c
