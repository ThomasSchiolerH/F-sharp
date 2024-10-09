// Problem 1
// Q1.1
let rec numberOf x ys =
    match ys with
    | [] -> 0
    | n::rest when x=n -> 1 + numberOf x rest
    | _::rest -> numberOf x rest

// Q1.2
let positionsOf x ys =
    let rec count x ys k =
        match ys with
        | [] -> []
        | n::rest when x = n -> k::(count x rest (k+1))
        | _::rest -> count x rest (k+1)
    count x ys 0

// Q1.3
let rec filterMap p f xs =
    match xs with
    | [] -> []
    | x::rest when p x -> f x::filterMap p f rest
    | x::rest -> filterMap p f rest

// Problem 2
// Q2.1
(*
    1. ([],[1;2;3])
    2. ([1;2;3],[4;5])
    3. ([1;2;3],[])
*)

// Q2.2
(*
    int -> 'a list -> 'a list * 'a list
*)

// Q2.3
(*
    It splits the list into two lists ([x1,....,xk],[xk+1,....xn])
*)

// Q2.4
(*
    x::xs1 is the last thing that is computed, so it is not
*)


// Problem 3
// Q3.1
let characteristicSeq p s =
    Seq.map (fun x -> if p x then 1 else 0) s

// Q3.2
let fracSeq s =
    Seq.mapi (fun j x -> (float x) / (float (j + 1))) s

// Q3.3
let accSum s =
    Seq.scan (fun acc x -> acc + x) 0 s

// Problem 4
type Volume = float
type Piece = A | B | C | Plastic of Volume | Glass of Volume

// Q4.1
let isWellFormed p =
    match p with
    | A -> true
    | B -> true
    | C -> true
    | Plastic(v) when v>0.0 -> true
    | Glass(v2) when v2>0.0 -> true
    | _ -> false 


// Q4.2
let rec isWellFormedList pl =
    match pl with
    | [] -> true
    | p::rest -> isWellFormed p && isWellFormedList rest

// Q4.3
let deposit p =
    match p with
    | A -> 1.0
    | B -> 1.5
    | C -> 3.0
    | Plastic(v) when v>0.5 -> 3.0
    | Plastic(v) -> 1.0
    | Glass(v) when v<1.0 -> 1.5
    | Glass(v) -> 3.0

// Q4.4
let rec totalDeposit pl =
    match pl with
    | [] -> 0.0
    | p::rest -> deposit p + totalDeposit rest

// Q4.5
let toSummary ps =
    let rec aux ps nrc nru =
        match ps with
        | [] -> (nrc, nru)
        | p::rest when p = A || p = B || p = C -> aux rest (nrc + 1) nru
        | p::rest -> aux rest nrc (nru + 1)
    aux ps 0 0

// Problem 5
type Concept = string;;
type Ontology = O of Concept*Classification
and Classification = Ontology list;;

// Q5.1
let o1 = O("Product",[O("DVD",[]);O("CD",[]);(O("Book",[O("Science",[]);O("Pocket",[])]))])

// Q5.2
let rec occursIn (c:Concept) (o:Ontology) =
    match o with
    | O(n,_) when n=c -> true
    | O(_,o) -> List.exists (occursIn c) o 

// Q5.3
let rec elementaryConcepts o =
    match o with
    | O(n,[]) -> [n]
    | O(_,cl) -> List.collect elementaryConcepts cl

// Q5.4
let rec find c o =
    match o with
    | O(n,_) when n=c -> Some O
    | O(_,o) -> if List.exists (find c) o then Some O else None

// Q5.5
(*
    find: Concept -> Ontology -> Option<Ontology>
*)



