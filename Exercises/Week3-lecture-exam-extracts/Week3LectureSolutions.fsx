// Example solutions to Week3 lecture exercises            Michael R. Hansen 04-09-2023

// 
// Problem 1 (Exam May 23, Problem 1) 
// 1.
let rec numberOf x ys = 
    match ys with                
    | [] -> 0
    | y::ys when y=x -> 1 + numberOf x ys
    | _::ys          -> numberOf x ys;;

// 2
let rec posOf x ys i = 
   match ys with
   | []               -> []
   | y::tail when x=y -> i::posOf x tail (i+1)
   | _::tail          -> posOf x tail (i+1)

let rec positionsOf x ys = posOf x ys 0;;

// 3
let rec filterMap p f xs = 
   match xs with 
   | []               -> []
   | x::tail when p x -> f x :: filterMap p f tail
   | _::tail          -> filterMap p f tail;;


// Problem 2 (Exam May 23 Problem 2 (1,2,3)

let rec splitAt i xs =
   if i<=0 then ([],xs)
   else match xs with 
        | []      -> ([],[])
        | x::tail -> let (xs1,xs2) = splitAt (i-1) tail
                     (x::xs1,xs2);;

// what is the value of:
// -  splitAt -1 [1..3];;
// answer   ([], [1; 2; 3])

// -  splitAt 3 [1..5];;
// answer   ([1; 2; 3], [4; 5])

// -  splitAt 4 [1..3]
// answer   ([1; 2; 3], [])

// What is the type of splitAt? 
// provide a brief justification
(* answer
From the form of the declaration: let rec splitAt i xs = ... , we see that the type of splitAt
must have the form: Ti -> Txs -> Tresult, where i: Ti, xs: Txs and Tresult is the type of the expression for the result.

From if i < 0 then ... we know that i has type int, that is, i: int.

From the expression 'match xs with ... ', we see from the patterns that xs must a list, that is, xs: 'a list, 
where 'a is a fresh type variable.

From the then-expression: ([], xs) we know that the value is a pair of two lists and the second list xs has
type a' list.  From the second clause in the match expression we have that x: 'a and therefore x::xs1 has type 'a list. 

Since the is no further type constraint, the most general type of splitAt is:
splitAt: int -> 'a list -> 'a list * 'a list 
*)

// What is splitAt computing?

(* splitAt k xs, where xs = [x_0; x_1; ...; x_(n-1)], is a list having n>=0 elements 
Three cases:
k<=0: splitAt k xs = ([],xs)
1<=k<n: splitAt k xs = ([x_0; x_1; ...; x_(k-1)],[x_k; ... x_(n-1)])
k>=n: splitAt k xs = (xs, [])

*)


// Problem 3 (Exam Dec. 2018 Problem 2 (1,2)

let rec f(xs,rs) = 
    match xs with
    | []   -> rs
    | [x]  -> x::rs
    | x1::x2::xs -> x1::f(xs,x2::rs)
let g xs = f(xs,[]);;


// Evaluation 
(*
Answer:
   g [1;2;3;4;5]
=> f([1;2;3;4;5],[])
=> 1::f([3;4;5],[2])
=> 1::3::f([5],[4;2])
=> 1::3::5::[4;2]
= [1;3;5;4;2]
*)


// What are the types of f and g:

(* answer:
f: 'a list * 'a list -> 'a list
g: 'a list -> 'a list

g [x_0; x_1; x_2 x_3 ; ... x_(n-1)] = [x_0; x_2; ... ; x_(n-1) ; ... x_3; x_1]

The result of g xs is the list that can be described as follows :
The first part of the result is obtained from xs by taking just the elements occurring at even positions (0, 2, ...).
The elements occurring in odd positions (1, 3, ...) of xs constitute the second (and last) part of the result. 
These elements occur in reversed order in the result. 
*)

