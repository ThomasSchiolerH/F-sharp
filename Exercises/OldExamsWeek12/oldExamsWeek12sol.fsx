// Problem 1 May 2018
// Q1.1
(*
    f [1;6;0;8] [0;7;3;3] evaluates to 
1::0::f [6;0;8] [7;3;3] evaluates to
1::0::6::7::f [0;8] [3; 3] evaluates to 
1::0::6::7::0::3::f [8] [3] evaluates to 
1::0::6::7::0::3::8::3::f [] [] evaluates to 
1::0::6::7::0::3::8::3::[] = [1;0;6;7;0;3;8;3] 
*)

// Q1.2
(*
    'a list -> 'a list -> 'a list
    takes head of each list and puts them together in a new
    list and does this recursively until at least one of them is
    empty
*)

// Q1.3


// Q1.4


// Problem 2.1 from May 2017
// Q 2.1
(*
    F: int -> int list : 
    f 5 = [5; 3; 1] as can by an evaluation
    f 5 evaluates to ("curly arrow" should be used as in the textbook)
    5::g 4 evaluates to 
    5::f 3 evaluates to 
    5::3::g 2 evaluates to
    5::3::f 1 evaluates to
    5::3::1::g 0 evaluates to
    5::3::1::[]

    H: seq<'a> -> ('a -> 'b) -> seq<'b> :
    h (seq [1;2;3;4]) (fun i -> i+10) = seq [11; 12; 13; 14]
*)

// Problem 3 May 2016
type Container =
       | Tank of float * float * float // (length, width, height)
       | Ball of float                 // radius
       | Cylinder of float * float     // radius, height

// Q3.1
let tank1 = Tank(5.0, 3.0, 2.0)
let ball2 = Ball 2.0

// Q3.2
let isWF c =
    match c with
    | Tank(v1,v2,v3) -> v1>=0.0 && v2>=0.0 && v3 >=0.0
    | Ball r1 -> r1>=0.0
    | Cylinder(x1,x2) -> x1>=0.0 && x2>=0.0


// Q3.3
let volume c =
    match c with
    | Tank(v1,v2,v3) -> v1*v2*v3
    | Ball r1 -> 4.0/3.0*System.Math.PI*r1*r1*r1
    | Cylinder(x1,x2) -> System.Math.PI*x1*x1*x2

// Q3.4
// Done

type Name     = string
type Contents = string
type Storage  = Map<Name, Contents*Container>

// Q3.5
let s1 = Map.ofList [("tank1", ("oil",tank1)); ("ball1",("water", ball2))]

// Q3.6
let find n stg =
    match Map.find n stg with
    | (cnt, c) -> (cnt, volume c)
    | _ -> failwith "no"




// Problem 4 May 2016
type T<'a> = L | N of T<'a> * 'a * T<'a>

// Q4.1
(*
    t<int>
*)
let t1 = L
let t2 = 


// Q4.2
let rec count' a t = 
    match t with
    | L -> 0
    | N(t1,v,t2) when v=a -> 1 + count' a t1 + count' a t2 
    | N(t1,v,t2) -> count' a t1 + count' a t2

let rec replace a b t =
    match t with
    | L -> L
    | N(t1,v,t2) when v=a -> N(replace a b t1,b, replace a b t2)
    | N(t1,v,t2) -> N(replace a b t1, v , replace a b t2)

