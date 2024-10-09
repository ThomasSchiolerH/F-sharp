// Solution to the set: Week10.pdf   MRH 08-10-2023 


(* Problem 2 from exam May 2021 *)

let rec ins x = function 
                | (y,n) :: ys when x=y -> (y,n+1)::ys                (* 1 *)
                | pair  :: ys          -> pair::ins x ys             (* 2 *)
                | []                   -> [(x,1)];;                  (* 3 *)


let rec cntBy f xs acc = match xs with 
                         | []      -> acc                            (* 4 *)
                         | x::rest -> cntBy f rest (ins (f x) acc);; (* 5 *)

let countBy f xs = cntBy f xs [];;                                   (* 6 *)

let cn = countBy (fun x -> x%2) [1..3]


(* Question 2.1   

Type for ins: 

From the form of the declaration 
   let rec ins x = function | ... ->  ... 

We see that the type of ins must have the form: 
  ins: 'a -> 'b -> 'c
where x: 'a and 'b and 'c are the argument and value types for the function expression function | .. -> ..

From the pattern (y,n)::ys when x=y in clause (1) we see 
- 'b must be a list of type: ('a*'d) list (where 'd is a new type variable)
  where n: 'd and y has the same type as x, that is 'a, because of the 'when x=y' condition
  and therefore 'a is constrained by: 'a: equality
- we see that the expression (y,n+1)::ys in clause (1) must have type 
  ('a*int) list because n must have type int, n: int, due to the expression n+1 and the type of ::
  and therefore 'd: int, 'c =  ('a*int) list and ins has the type 
       ins: 'a -> ('a*int) list -> ('a*int) list when 'a: equality
  according to clause (1).

This type is consistent with the clauses (2) and (3). Since there are no further constraints
   ins: 'a -> ('a*int) list -> ('a*int) list when 'a: equality
is the most general type of ins.
  
Type for cntBy:

From the form of the declaration: 
    let rec cntBy f xs acc = match xs with | [] -> acc | ... 
we see that the type of cntBy must have the form 

  cntBy: 'a -> 'b list -> 'c -> 'c 

where f: 'a, xs: 'b list, acc:'c since xs is matched with a list pattern [] and the expression in clause (4) is acc. 

From the pattern in clause (5) we get x: 'b and rest: 'b list. 
From the expression f x we get 
- f is a function f: 'b -> 'd, where 'd is new, f x: 'd and 'a = 'b -> 'd  

The type of ins can be instantiated to ins: 'd -> ('d*int) list -> ('d*int) list when 'd:equality (matching that f x: 'd)),
therefore from the expression ins (f x) acc we get we get that acc: ('d*int) list with the constraint 'd: equality
Hence, 'c = ('d*int) list with the constraint 'd: equality

Since there are no further constraints we summerize the most general type of cntBy as:

  cntBy: ('b -> 'd) -> 'b list -> ('d*int) list -> ('d*int) list when 'd: equality

By renameing of type variables ('b becomes 'a and 'd becomes 'b)  we get 

  cntBy: ('a -> 'b) -> 'a list -> ('b*int) list -> ('b*int) list when 'b equality
*)

(* Question 2.2   

let f = fun x -> x%2 in the following evaluation

    countBy [1..3]
 => cntBy f [1;2;3) []                        using (* 6 *)
 => cntBy f [2;3] (ins (f 1) [])              using (* 5 *)
 => cntBy f [2;3] (ins 1 [])                  evaluate f 1
 => cntBy f [2,3] [(1,1)]                     using (* 3 *)
 => cntBy f [3] (ins (f 2) [(1,1)])           using (* 5 *)
 => cntBy f [3] (ins 0 [(1,1)])               evaluate f 2
 => cntBy f [3] ((1,1)::ins 0 [])             using (* 2 *)
 => cntBy f [3] ((1,1)::[(0,1)])              using (* 3 *)
 => cntby f [] (ins (f 3) [(1,1); (0,1)])     using (* 5 *)  
 => cntby f [] (ins 1 [(1,1); (0,1)])         evaluate f 3  
 => cntby f [] [(1,2); (0,1)]                 using (* 1 *)
 => [(1,2); (0,1)]                            using (* 4 *)
*)


// Problem 2 from exam December 2021  

type T = | One of int | Two of int * T * int * T 

let rec f p t = 
   match t with 
   | One v when p v           -> [v]        (* C 1 *)
   | Two(v1,t1,_,_) when p v1 -> v1::f p t1 (* C 2 *)
   | Two(_,_,v2,t2)           -> v2::f p t2 (* C 3 *)
   | _                        -> [];;       (* C 4 *)

(*
Q1: 

The type of f is: (int -> bool) -> T -> int list

Given f p t: 

let a path through t guided by p be a list of nodes described as follows:
- the root node is the start.
- for a node of the form: Two(v1,t1,v2,t2) the left subtree t1 is the next node if p v1 holds, otherwise t2 is the next node
- the path ends in a leaf node of the form One v

The value of f p t is the list of integers obtained from the path described above by as follows
- select v1 from a node Two(v1,t1,v2,t2) if the left subtree t1 is the next node; otherwise select v2
- for a leaf node One v, select v only if p v holds.

*)

let p1 n = n%2=0;;
let p2 n = n%2<>0;;

let t = Two(2, One 0, 3, One 4);;

(* Q2 

Possible test descriptions in terms of p1, p2 and t declared  above.

Value for p  | Value for t | Expected value: f p t | Clauses exercised
-----------------------------------------------------------------------
 p1          | t           | [2;0]                 | C1 C2
 p2          |             | [3]                   | C3 C4

*)



// Problem 3 from the exam December 2021

type Trie<'a> = N of 'a * bool * Children<'a> 
and Children<'a> = Trie<'a> list 

let t1 = N(0, false, [N(0, false, [N(1,true,[])])]);;
let t2 = N(0, true, [N(0, false, [N(1,true,[])])]);;

let ta = N(1,true,[N(2,true,[])]);;
let tb = N(3,false,[N(0,true,[])]);;
let tc = N(2,true,[]);;
let t3 = N(0,false, [ta;tb;tc]);;

// Q1
let rec numberOfNodes(N(_,_,ts)) = 1+numberOfNodesCh ts
and numberOfNodesCh = 
   function 
   | []    -> 0 
   | t::ts -> numberOfNodes t + numberOfNodesCh ts;;


// Q2
// accept has type 'a list -> Trie<'a> -> bool when 'a: equality 
 let rec accept xs t =
    match (xs,t) with
    | ([x], N(y,true,_)) when x=y -> true
    | (x::xs, N(y,_,ts)) when y=x -> List.exists (accept xs) ts
    | _                           -> false;;

// Q3
let rec wordsOf = 
   function 
   | N(x,true,ts)  -> Set.union (Set.singleton [x]) 
                                (Set.map (fun xs -> x::xs) (wordsOfList ts))
   | N(x,false,ts) -> Set.map (fun xs -> x::xs) (wordsOfList ts)                                  
and wordsOfList = 
   function
   | []    -> Set.empty
   | t::ts -> Set.union (wordsOf t) (wordsOfList ts);;


// Q 4
let rec containsUseless t = 
    match t with
    | N(_,false,[]) -> true   
    | N(_,_,ts)     -> List.exists containsUseless ts;;

// Q 5
let rec degree(N(_,_,ts)) = degreeList (List.length ts) ts
and degreeList a = function   
                   | []    -> a
                   | t::ts -> degreeList (max a (degree t)) ts;;


