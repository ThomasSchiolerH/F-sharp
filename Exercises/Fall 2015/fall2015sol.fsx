// Problem 1

type Appliance = string
type Usage     = Appliance * int
let ad1 = ("washing machine", 2)
let ad2 = ("coffee machine", 1)
let ad3 = ("dishwasher", 2)
let ats = [ad1; ad2; ad3; ad1; ad2]

// Q1.1
let rec inv ats =
    match ats with
    | [] -> true
    | (_,x)::rest when x>=0 -> inv rest
    | _ -> false

// Q1.2
let rec durationOf (a: Appliance) ats=
    match ats with
    | [] -> 0
    | (a1,x)::rest when a1=a -> x + durationOf a rest
    | (a1,x)::rest -> durationOf a rest

// Q1.3
let isWellFormed ats =
    inv ats && List.forall (fun (a, _) -> durationOf a ats <= 24) ats

// Q1.4 
let rec delete a ats =
    match ats with
    | [] -> []
    | (a',_)::rest when a=a' -> delete a rest
    | (a',t)::rest -> (a',t)::delete a rest

// Q1.5

type Price  = int
type Tariff = Map<Appliance, Price>

let isDefined ats trf =
    List.forall (fun (a, _) -> Map.containsKey a trf) ats

// Q1.6
let rec priceOf ats trf =
    match ats with
    | [] -> 0
    | (a',t)::rest when Map.containsKey a' trf -> 
        let price = Map.find a' trf
        t*price + priceOf rest trf
    | _ -> failwith "No tariff found"


// Problem 2 

let rec g1 p = function
    | x::xs when p x -> x :: g1 p xs
    |_ -> [];;
let rec g2 f h n x =
  match n with
  | _ when n<0 -> failwith "negative n is not allowed"
  | 0          -> x
  | n          -> g2 h f (n-1) (f x);;

// Q2.1
// Type of g1:
// ('a -> bool) -> a'list -> a'list
// Type of g2:
// ('a -> 'a) -> ('a -> 'a) -> int -> 'a -> 'a

// Q2.2
// tail-recursive:
let rec g1_tail_recursive p acc = function
    | x::xs when p x -> g1_tail_recursive p (x :: acc) xs
    | _::xs -> []
    | [] -> List.rev acc

// Continuation based
let g1_continuation p xs =
    let rec g1_cont xs cont = 
        match xs with
        | x::xs' when p x -> g1_cont xs' (fun ys -> cont (x::ys))
        | _::xs' -> cont []
        | [] -> cont []
    g1_cont xs (fun x -> x)

// f1, f2, f3
let f1 m n k = seq { for x in [0..m] do
                       for y in [0..n] do
                          if x+y < k then
                             yield (x,y) };;

let f2 f p sq = seq { for x in sq do
                        if p x then
                           yield f x };;

let f3 g sq = seq { for s in sq do
                      yield! g s };;

// Q2.4

// 0,0
// 0,1
// 0,2
// 1,0
// 1,1
// 2,0

// Q2.5
let f2s f p sq = 
    sq |> Seq.filter p |> Seq.map f
// Seq.filter p filters the elements of sq to only keep those that satisfy the predicate p.
// Seq.map f then transforms each of those elements using the function f.
// |> means "pass it to"

// Q2.6

// f1: int -> int -> int -> seq<int*int>
// f2: ('a -> 'b) -> ('a -> bool) -> seq<'a> -> seq<'b>
// f3: ('a -> 'b/seq<'b>) -> seq<'a> -> seq<'b>
// f1: computes all tuples where both elements (m,n) added together is less than k
// f2: looks at each element in the sequence sq. If they satisfy the predicate p, then the function f is applied to the element.
// f3: for each element s in the sq it applies function g to produce a sequence.

// Q3
type Name = string
type Flow = int //can be assumed positive
type River = R of Name * Flow * Tributaries
and  Tributaries = River list

// Q3.1
let riv3 = R("R3",8,[])
let riv1 = R("R1", 5, [])  // For the sake of completeness, declaring R1
let riv4 = R("R4", 2, [])
let riv2 = R("R2", 15, [riv4])
let riv = R("R",10,[riv1;riv2;riv3])

// Q3.2
let rec contains n r =
    match r with
    | R(x,_,_) when x=n -> true
    | R(_,_,trib) -> 
        List.exists (contains n) trib

// Q3.3
let rec allNames r =
    match r with
    | R(n,_,trib) -> 
        n :: (List.collect allNames trib)

// Q3.4
let rec totalFlow r =
    match r with
    | R(_,f,trib) -> f + (List.sumBy totalFlow trib)

// Q3.5

