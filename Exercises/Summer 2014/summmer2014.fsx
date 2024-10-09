// Problem 1

// Q1.1
// f: f 2 3 -> 2 in the power of 4
// 2 * f(2 3)
// 2* 2* f(2 2)
// 2* 2* 2 f(2 1)
// 2* 2* 2* 2 *1 = 16

// g: g (fun x -> x>2) (fun x -> x*2) [1;2;3;4]            output: [6;8]

// h: h (B "Hello, World!")
// If n is an integer it gets turned into a string
// If s is a string it returns a string
// If the input is C (two parts) it looks at if it is an integer or string 
// and converts into string
//example: h (C(A 1, B "Hello"));;          output: "1Hello"

// Q1.2
// f: a' -> a' -> b'
// g: (a' -> bool) -> (a' -> b') -> a' list -> b' list
// h: T -> string

// Q1.3
let rec f acc n = function 
    | 0 -> acc
    | k when k > 0 -> f (n * acc) n (k - 1)
    | _ -> failwith "illegal argument"

// Q1.4
// sq: Seq<int>
// k: int -> Seq<int*int>

// sq: computes 3*0, 3*1, 3*2, 3*3.... infinitely
// k: computes pairs (0, 0-j), (1, 1-j), (2,2-j), (3, 3-j)

// Q1.5
// xs:
// Seq.take 4 sq takes the first 4 elements of sq. The Seq.toList
// puts the first 4 values into a list.
// Output: [0,3,6,9]

// ys:
// Same as xs, takes the first 4 values of k 2
// Output: [(0,-2),(1,-1),(2,0),(3,1)]

// Problem 2
// Q2.1
let rec ordered xs =
    match xs with
    | [] -> true
    | [_] -> true
    | x::x1::rest when x<=x1 -> ordered (x1::rest)
    | x::x1::rest -> false 

// Type: int list -> bool

// Q2.2
let rec smallerThanAll x xs =
    match xs with
    | [] -> true
    | n::rest when x<n -> smallerThanAll x rest
    | n::rest -> false

// Type: int -> int list -> bool

// Q2.3
let rec insertBefore p x xs =
    match xs with
    | [] -> []
    | n::rest when p n -> x::n::rest
    | n::rest -> n::insertBefore p x rest

// Q2.4

type Sex =  | M              // male
            | F              // female

let sexToString s =
    match s with
    | M -> "Male"
    | F -> "Female"

// Q2.5
let rec replicate n str =
    match n with
    | _ when n<0 -> failwith "No negative"
    | 0 -> ""
    | n -> str + replicate (n-1) str


// Problem 3
type Name = string;;
type YearOfBirth = int;;
type FamilyTree = P of Name * Sex * YearOfBirth * Children
and Children = FamilyTree list;;

// Q3.1
let rec isWF ft =
    match ft with
    | P(_,_,yob, c)


