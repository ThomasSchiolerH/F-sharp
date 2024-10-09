// Problem 1
// Q1.3



// Problem 2
type T = Leaf of char | Branch of T*T
// Q2.1
let t0 = Branch(Leaf 'a', Branch(Leaf 'b', Leaf 'c'));;
let rec toList t =
    match t with
    | Leaf s -> [s]
    | Branch(tl,tr) -> toList tl @ toList tr

// Q2.2
let legal t =
    let list = toList t
    List.length list >= 2 && list = List.distinct list

// Q2.3
type Dir =  |L //goleft
            |R;; //goRight
type Code = Dir list;;
type CodingTable = Map<char, Code>;;

let rec encode ct cl =
    match cl with
    | [] -> []
    | c::cl1 -> Map.find c ct::encode ct cl1

// Q2.4
let rec ofT t =
    let l1 = toList t
    encode 


