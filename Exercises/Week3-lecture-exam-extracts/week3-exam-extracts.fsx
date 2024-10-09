// Problem 1

// Q1.1
let rec numberOf x ys: int = 
    match ys with
    | [] -> 0
    | y::rest when y = x -> 1 + numberOf x rest
    | _::rest -> numberOf x rest

numberOf 2 [0;2;3;3;0;2;4;2;1]

// Q1.2

let positionsOf x ys =
    let rec positionsOfHelper count xs =
        match xs with
        | [] -> []
        | y::rest when y = x -> count :: positionsOfHelper (count + 1) rest
        | _::rest -> positionsOfHelper (count + 1) rest
    positionsOfHelper 0 ys

positionsOf 2 [0;2;3;3;0;2;4;2;1]

// Q1.3
// let rec filterMap p f xs =
//     match xs with
//     | [] -> []
//     | _::rest when !(bool) -> filterMap p f rest
//     | x::rest when bool -> x::filterMap p f rest

let rec filterMap p f xs =
    match xs with
    | [] -> []  // Base case: Return an empty list when the input list is empty
    | x::rest when p x -> (f x) :: filterMap p f rest  // Apply f to x if it satisfies the predicate p
    | _::rest -> filterMap p f rest  // Skip elements that don't satisfy the predicate p

filterMap (fun x -> x>=2) string [0;2;3;3;0;2;4;2;1]

// Problem 2

// Q2.1
let rec splitAt i xs =
      if i<=0 then ([],xs)
      else match xs with
           | []      -> ([],[])
           | x::tail -> let (xs1,xs2) = splitAt (i-1) tail
                        (x::xs1,xs2);;

splitAt -1 [1;2;3] // output = ([],[1;2;3])
splitAt 3 [1;2;3;4;5] //output = ([1;2;3],[4;5])
splitAt 4 [1;2;3] // output = ([1;2;3],[])

// Q2.2
// a' -> a'list -> (a'list->a'list)

// Q2.3
// It finds the index k in the list, and takes the first element of the list and
// stores it in a new list until it reaches the index k. Xs is therefore split up
// into two lists, from index 0 up to but not including index k.
// The value [x0,x1, ..., x_k-1],[x_k, ... , x_n-1]

// Problem 3

// Q3.1
// 
let rec f(xs,rs) = 
                    match xs with
                    | []   -> rs
                    | [x] -> x::rs
                    | x1::x2::xs -> x1::f(xs,x2::rs)
let g xs = f(xs,[]);;    
g [1;2;3;4;5]
//1,3,5 4,2
// ,2

// Q3.2
// f: a' list * a' list -> a'list
// g: a' list -> a' list 