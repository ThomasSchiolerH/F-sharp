// Michael R. Hansen 16-05-2022
//                   15-11-2023
// Exam set in 02157 from from May 2022 
// Covered by Week11.pdf, week10.pdf and Week8.pdf



// Problem 1

type Book  = string

type Shelf = Book list   // ordered alphabetically

type Date  = int 
type Name  = string
type Loan  = Book * Name * Date


let sh0 = ["Introduction to meta-mathematics"; 
           "To mock a mockingbird"; 
           "What is the name of this book"];;

let lns0 = [("h", "Bob", 4); 
            ("E", "Mary", 7);
            ("O", "Dick", 1)];;

// Q1
let rec onShelf b bs =
   match bs with
   | b':: rest when b'< b -> onShelf b rest
   | b'::_     when b'=b  -> true
   | _                    -> false;;

// Q2
let rec toShelf b bs = 
   match bs with 
   | []   -> [b]
   | b'::_ when b<=b' -> b::bs
   | b'::rest         -> b'::toShelf b rest;;

// Q3
let rec fromShelf b bs =
   match bs with
   | b'::rest when b' < b -> match fromShelf b rest with
                             | Some bs' -> Some (b'::bs') 
                             | None     -> None
   | b'::rest when b=b'   -> Some rest
   | _                    -> None;;

// Q4
let addLoan p n d ls = (p, n,d)::ls;; 

let rec removeLoan p n bs =
    match bs with 
    | []              -> [] 
    | (p',n',d)::rest -> if p=p' && n=n' then rest
                         else (p',n',d)::removeLoan p n rest;;

// Q5
let rec reminders d ls =
   match ls with 
   | []                         -> []
   | (b1,n1,d1)::rest when d1<d -> (n1,b1) :: reminders d rest
   | __::rest                   -> reminders d rest;;

// Q6

let f (n,b) = "Dear " + n + "!\n Please return \"" + b + ".\"\n Regards Robin";;  
let rec toLetters =
      function
      | []        -> []
      | pair::nbs -> f pair :: toLetters nbs;; 

// Q7
let reminders' d ls = List.foldBack (fun (b1,n1,d1) state -> if d1<d then (n1,b1)::state else state) ls [];; 

let toLetters' nbs = List.map f nbs;;    


// Problem 2

let rec skipWhile p = function 
                      | x::xs when p x -> skipWhile p xs
                      | xs             -> xs;;

let rec takeWhile p = function 
                      | x::xs when p x -> x::takeWhile p xs (* C1 *)
                      | _              -> [];;              (* C2 *)


let diff5 n = n<>5;;



// Q1 
(* 
Type of takeWhile

Due to the declaration form: 'let rec takeWhile p = function | x::xs ... x::takewhile ... | ... ' 
we know that the type of takeWhile has the form:
   'p -> ('a list -> 'a list) 
 where p: 'p and x:'a. The argument type and the value type of the function expression must be the same 
 list type (that is 'a list) due to the pattern x::xs and expression x::takeWhile p xs in C1. 
 Due to the 'when p x' considition in the pattern of C1 we know that p: 'a -> bool (i.e. 'p = 'a -> bool). 
 
 Since there are no further constraints, the most general type of takeWhile is
   ('a -> bool) -> 'a list -> 'a list
*)

// Q2
(*
   skipWhile diff5 [2;6;5;1;5;6]
=> skipWhile diff5 [6;5;1;5;6]
=> skipWhile diff5 [5;1;5;6]
=> [5;1;5;6].
*)

// Q3
(*
let xs be [x1; x2; ...; xn], where n>=0. 

Concerning skipWhile: There are two cases
1. skipWhile p xs = [xi; ...; xn] if p xi = false and p x is true for every element x in [x1; x2; ...;x(i-1)], where 1<= i <= n
2. skipWhile p xs = [] if p x is true for every element x in xs. (Covers also the case when n=0, i.e. xs = [])

Concerning takeWhile: There are two cases
1. takeWhile p xs = [x1; ...; x(i-1)] if p xi = false and p x is true for every element x in [x1; x2; ...;x(i-1)], where 1<= i <= n
2. takeWhile p xs = xs if p x is true for every element x in xs. (Covers also the case when n=0, i.e. xs = [])
*)

// Q4
(*
SkipWhile is a tail-recursive function because the only recursive call in the body of 
the function is in a tail position of a function expression: ' x::xs when p x -> skipWhile p xs'. 
When that recursive call returns, there no operations to be performed in the body of the function. 

takeWhile is NOT tail recursive. 

The recursive call of takeWhile occurs in a position ' ... x::takeWhile p xs ' that is not a tail position. 
When when the recursive call returns, the cons operation '::' still needs to be performed.

A tail-recursive variant of takeWhile is:
*)

let rec takeWhileA acc p = function 
                           | x::xs when p x -> takeWhileA (x::acc) p xs 
                           | _              -> List.rev acc;;

let takeWhile1 p xs = takeWhileA [] p xs 


// Problem 3

// Q1
let flip sq = Seq.map (fun (x,y) -> (y,x)) sq

// Q2
let dia n = seq {for i in seq [0 .. n]
                      do yield (i,n-i) }

// Q3
let allCoordinates  = 
    seq { for n in Seq.initInfinite (fun i -> 2*i)  do  
             yield! dia n
             yield! flip(dia (n+1))
        };;


// Problem 4

type T = Leaf of char | Branch of T*T;;


// Q1

let t0 = Branch(Leaf 'a', Branch(Leaf 'b', Leaf 'c'));;

let rec toList = function | Leaf c -> [c]
                          | Branch(t1,t2) -> toList t1 @ toList t2;;

// 2
let legal t = let cs = toList t 
              List.length cs > 1 && cs = List.distinct cs;;



type Dir  = |  L   // left 
            | R;; // right 
type Code = Dir list;;

type CodingTable = Map<char, Code>


// Q3
let rec encode m = function | []    -> []
                            | c::cs -> Map.find c m @ encode m cs;;

let encode' m cs = List.collect (fun c -> Map.find c m) cs;;


// let m = Map.ofList [('a', [L]); ('b', [R;L]) ; ('c', [R;R])];;

// Q4
let ofT t0 = 
    let rec loop t ds m = 
            match t with 
            | Leaf c        -> Map.add  c (List.rev ds) m
            | Branch(t1,t2) -> loop t2 (R::ds) (loop t1 (L::ds) m)      
    loop t0 [] Map.empty;;
                                             

// Q5
let rec firstCharOf t ds = 
   match (ds,t) with 
   | (ds, Leaf c)                                    -> (c,ds)
   | (L::ds', Branch(t',_)) | (R::ds', Branch(_,t')) -> (firstCharOf t' ds')
   | _                                               -> failwith "illegal prefix"

let rec decode t ds = match ds with 
                      | []   -> []
                      | _    -> let (c,ds') = firstCharOf t ds
                                c::decode t ds';;


