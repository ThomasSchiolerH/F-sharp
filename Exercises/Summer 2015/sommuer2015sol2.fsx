// Problem 1
// Q1.1
let rec repeat s n =
    match n with
    | 0 -> ""
    | n when n>0 -> s + repeat s (n-1)
    | _ -> failwith "Negative"

// Q1.2
let rec f s1 s2 n =
    match n with
    | 0 -> ""
    | n when n%2=1 -> f s1 s2 (n-1) + s1 + "\n"
    | n when n%2=0 ->  f s1 s2 (n-1) + s2 + "\n"
    | _ -> failwith "Negative"

// Q1.3
let rec viz m n =
    match m,n with
    | _,0 -> ""
    | m,n when m>0 && n>0 && n%2=1 -> viz m (n-1) + repeat "XO" m + "/n"
    | m,n when m>0 && n>0 && n%2=0 -> viz m (n-1) + repeat "OX" m + "/n"
    | _ -> failwith "Negative"
    
// Q1.4
let rec repeat' (s:string) n acc =
    match n with
    | 0 -> acc
    | n when n>0 -> repeat' s (n-1) (s+acc)
    | _ -> failwith "Negative"

let rec repeatC' (s:string) n acc cont =
    match n with
    | 0 -> cont acc
    | n when n>0 -> repeatC' s (n-1) (s+acc) cont
    | _ -> failwith "Negative"

// Problem 3
type Tree<'a> = Lf | Br of Tree<'a> * 'a * Tree<'a>;;

let t = Br(Br(Br(Lf,1,Lf),2,Br(Lf,3,Lf)),4,Br(Br(Lf,5,Lf),6,Br(Lf,7,Lf)));;

// Q3.1
let rec reflect t =
    match t with
    | Lf -> Lf
    | Br(t1,a,t2) -> Br(reflect t2,a, reflect t1)

// Q3.2 -????
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
(*
    K: int -> Tree<'a> -> Tree<'a> : i*a and every depth of branch is i^2*a
    Q: int -> Tree<'a> -> 'a list

    H:   int -> int -> bool -> Tree<'a'> -> 'a list
    returns a list of values that are all at depth n-m

     Q:   int -> Tree<'a> -> 'a list
     calls function h with a target depth of n and start 
     depth of n, so only looks at the n-th level and returns that value in a list
*)

// Problem 4
type CourseNo = int;;
type Title = string;;
type ECTS = int;;
type CourseDesc = Title*ECTS;;

type CourseBase = Map<CourseNo, CourseDesc>;;

// Q4.1
let isValidCourseDesc cd =
    match cd with
    | (_,e) when e%5=0 && e>0 -> true
    | _ -> false

// Q4.2
let isValidCourseBase cb =
    Map.forall (fun _ cd -> isValidCourseDesc cd) cb

// Q4.3
type Mandatory   = Set<CourseNo>;;
type Optional    = Set<CourseNo>;;
type CourseGroup = Mandatory * Optional;;

let disjoint s1 s2 =
    Set.isEmpty (Set.intersect s1 s2)

// Q4.4
let sumEcts cs cb = Seq.sum (Set.toSeq (Set.map (fun x-> snd (Map.find x cb)) cs))

// Q4.5