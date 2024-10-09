let rec fact n =
    if n=0 then 1
    else n * fact(n-1);;

fact 3

let rec add (Poly1:int list) (Poly2 : int list) =
    match Poly1,Poly2 with
    | [],[] -> []
    | [], _ -> Poly2
    | _, [] -> Poly1
    | x::Poly1, y::Poly2 -> (x+y):: add Poly1 Poly2

add [1;2] [3;4;5;6]

let rec mulC n (Poly:int list) : int list =
    match n,Poly with
    | 0, [] -> []
    | 0, _ -> []
    | n, [] -> []
    | n, x::Poly -> (n*x)::mulC n Poly

mulC 2 [2;0;0;1]

let rec sub (Poly1:int list) (Poly2:int list) =
    match Poly1,Poly2 with
    | [],[] -> []
    | _, [] -> Poly1
    | [], z::Poly2 -> (-1*z)::sub Poly1 Poly2
    | x::Poly1, y::Poly2 -> (x-y)::sub Poly1 Poly2

sub [1;2] [3;4;5;6]

let rec mulX (Poly:int list) :int list = 
    match Poly with
    | [] -> []
    | _ -> 0::Poly

mulX [2;0;0;1]

// let rec mul (Poly1:int list) (Poly2:int list) =
//     match Poly1,Poly2 with
//     | [],_ -> []
//     | _,[] -> []
//     | x::Poly1, y::Poly2 -> (x*y)::mul Poly1 Poly2

// mul [2;3;0;1] [1;2;3]




// 2016 Fall exam
type Name = string
type Event = string
type Point = int
type Score = Name * Event * Point
type Scoreboard = Score list
let sb = [("Joe", "June Fishing", 35); ("Peter", "May Fishing", 30);
             ("Joe", "May Fishing", 28); ("Paul", "June Fishing", 28)];;
//Q1.1
let rec inv = function 
              | []                       -> true
              | [(n,e,p)] -> p>= 0 
              | (n,e,p)::(n1,e1,p1):: sb -> p>=p1 && inv ((n1,e1,p1)::sb)

//Q1.2

(*
let rec insert s sb =
    match sb with
    | [] -> [s]
    | y::_ when s <= y -> s::sb
    | y::ytail -> y::insert s ytail
*)
let rec insert (s: Score) (sb: Scoreboard) =
    match sb with
    | [] -> [s]
    | (n, e, p) :: rest when s >= (n, e, p) -> s :: (n, e, p) :: rest
    | (n, e, p) :: rest -> (n, e, p) :: insert s rest


let updatedScoreboard = insert ("Bob", "June Fishing", 10) sb

// Print the updated scoreboard
let printScoreboard (scoreboard: Scoreboard) =
    scoreboard |> List.iter (fun (name, event, points) ->
        printfn "%s - %s: %d" name event points
    )

printScoreboard updatedScoreboard

// Q1.3  get: Name*Scoreboard -> (event*Point) list

let rec get (n : Name, sb : Scoreboard) : (Event*Point) list  =
    match n,sb with
    | _,[] -> []
    | "", _ -> []
    | n, (n1,e1,p1)::rest when n=n1 -> (e1,p1)::get(n,sb)
    | n,_::rest -> get(n,rest)

// Q1.4  top: int -> Scoreboard -> Scoreboard option.
let rec top (k:int) (sb:Scoreboard) : Scoreboard option =
    match k,sb with
    | k,_ when k<0 -> None
    | k,_ when k=0 -> Some []
    | _,[] -> None
    //| k,(n,e,p)::rest -> p::top (k-1) rest
    //| k, (n, e, p) :: rest -> top (k-1) rest
    | k, (n, e, p) :: rest -> match top (k-1) rest with
                                        | None -> None
                                        | Some sb' -> Some ((n,e,p)::sb')

(*
In functional programming languages like F#, Some is a type constructor for option types. Some is used to wrap a value when you want
to indicate that a value exists or is present. It's often used in conjunction with None to represent optional
values or values that may or may not exist.
In the context of your code:

Some sb' means that sb' is a value that exists, and it is wrapped in the Some constructor.

None means that there is no value present, indicating the absence of a value.

So, when you return Some sb', you are saying that there is a Scoreboard value sb' present and it is being wrapped 
in the Some option type. This is a way to represent that the computation or result was successful and produced a valid Scoreboard.

Conversely, when you return None, you are indicating that there is no valid Scoreboard value to return, often due to some condition not being met.

Option types (combining Some and None) are a common way to handle scenarios where a value may or may not be present, 
which is useful for handling errors, optional results, or partial data.
*)

let updatedScoreboard2 = top 3 sb

// Print the updated scoreboard
let printScoreboard2 (scoreboard: Scoreboard) =
    scoreboard |> List.iter (fun (name, event, points) ->
        printfn "%s - %s: %d" name event points
    )

printScoreboard updatedScoreboard



// Q2.1
(*Declare a function replace a b xs that gives the list obtained
from xs by replacing every occurrence of a by b. For example, replace 2 7 [1; 2; 3; 2; 4]=[1; 7; 3; 7; 4].*)
let rec replace (a:int) (b:int) (xs:int list) = 
    match xs with
    | [] -> []
    | x::rest when a=x -> b::replace a b rest
    | x::rest -> replace a b rest

// Q2.2
//Give the (most general) type of replace.
(* 
int->int->int list->int list is not general enough
real answer: 'a -> 'a -> 'a list -> 'a list
*)

// Q2.3
(*
    It is not tail-recursive because there are additional operations to be performed after the recursive calls. Specifically, 
    in the case where a is equal to x, you add b to the result list (b::replace a b rest) after making the recursive call.
*)
// TODO: MAKE IT TAIL RECURSIVE

// Q3.1
// TODO:

// Q4.1

