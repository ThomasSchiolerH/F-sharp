// Q1.1
let rec repeat (s:string) n:string =
    match n with
    | 0 -> ""
    | n when n>=1 -> s + repeat s (n-1)
    | _ -> failwith "Do not use negative integer"

// Q1.2
let rec f (s1: string) (s2: string) n : string =
    match n with
    | 0 -> ""
    | 1 -> s1
    | n when n > 0 -> 
        if n % 2 = 0 then 
            f s1 s2 (n - 1) + "\n" + s2
        else 
            f s1 s2 (n - 1) + "\n" + s1
    | _ -> failwith "No negative numbers"


// Q1.3
let viz m n =
    let ox = repeat "OX" m
    let xo = repeat "XO" m
    f xo ox n

// Q1.4
// TODO:

// Q2.1
let rec mixMap f = function
    | (x::xs,y::ys) -> (f x y)::mixMap f (xs, ys)
    | ([],[]) -> []
    | _ -> failwith "mix: parameter error";;

// Q2.2
let rec unmixMap f g = function
| [] -> ([], [])
| (x, y)::tail ->
    let fx, gy = unmixMap f g tail
    ((f x)::fx, (g y)::gy);;

// Q2.3 
// Look at comments above

// Q3
type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;
let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;

// Q3.1

let rec reflect = function
    | Lf -> Lf
    | Br(left, x, right) -> Br(reflect right, x, reflect left);;

// Q3.2
let accumulate tree =
    let rec aux acc = function
        | Lf -> Lf
        | Br(left, value, right) ->
            let newAcc = acc + value
            let newLeft = aux newAcc left
            let newRight = aux newAcc right
            Br(newLeft, newAcc, newRight)
    aux 0 tree

// Q3.3
// K:   int -> Tree<int> -> Tree<int>
// Value (a) of each node is timed by i, by each step down in the tree the i is squared

// H:   int -> int -> bool -> Tree<'a'> -> 'a list
// returns a list of values that are all at depth n-m

// Q:   int -> Tree<'a> -> 'a list
// calls function h with a target depth of n and start 
// depth of n, so only looks at the n-th level and returns that value in a list


// Q4
type CourseNo = int
type Title = string
type ECTS = int
type CourseDesc = Title * ECTS
type CourseBase = Map<CourseNo, CourseDesc>

// Q4.1
let isValidCourseDesc desc = 
    match desc with
    | (_,ects) -> ects>0 && ects%5=0

// Q4.2
let isValidCourseBase cb =
    Map.forall (fun _ desc -> isValidCourseDesc desc) cb


type Mandatory   = Set<CourseNo>
type Optional    = Set<CourseNo>
type CourseGroup = Mandatory * Optional

// Q4.3
let disjoint s1 s2 =
    Set.isEmpty (Set.intersect s1 s2)

// Q4.4
let sumECTS cs cb =
    Set.fold (fun acc courseNo ->
        match Map.tryFind courseNo cb with
        | Some (_, ects) -> acc + ects
        | None -> acc
    ) 0 cs

