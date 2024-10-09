// Q1

type Rel<'a,'b> = ('a * 'b list) list
let rel: Rel<int,string> = [(1, ["a"; "b"; "c"]); (4,["b"; "e"])];;

// Q1.1
let rec apply x rel = 
    match rel with
    | [] -> []
    | (n,rel)::rest when x = n -> rel
    | _::rest -> apply x rest;;

// Q1.2
let inRelation x y rel =
    List.contains y (apply x rel);;

// Q1.3
let rec insert x y rel =
    match rel with
    | [] -> [(x, [y])]
    | (xs, ys)::rest when xs = x -> (x, y :: ys) :: rest
    | (xs, ys)::rest -> (xs, ys) :: insert x y rest

// Q1.4
let toRel list =
    List.fold (fun acc (x, y) -> insert x y acc) [] list;;




// Problem 4
type Outcome  = | S | F 
type Sample   = Outcome list
type ProbTree = | Branch of string * float * ProbTree * ProbTree
                | Leaf of string
// Q4.1
let rec probOK pt =
    match pt with
    | Leaf _ -> true
    | Branch(_,p,lt,rt) when p>=0.0&&p<=1.0 -> probOK lt && probOK rt
    | _ -> false

// Q4.2
let rec isSample os t =
    match t with
    | 