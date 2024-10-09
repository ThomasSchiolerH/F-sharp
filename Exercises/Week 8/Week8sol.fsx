// Problem 1
type Book = string
type Shelf = Book list

type Date = int
type Name = string
type Loan = Book * Name * Date

let sh0 = ["Introduction to meta-mathematics";
                   "To mock a mockingbird";
                   "What is the name of this book"];;

let ls0 = [("Communication and concurrency", "Bob", 4);
                   ("Programming in Haskell", "Paul", 2);
                   ("Communicating Sequential processes", "Mary", 7);
                   ("Elements of the theory of computation", "Dick", 1)];;
// Q1.1
let rec onShelf b s=
    match s with
    | [] -> false
    | b1::rest when b1=b -> true
    | b1::rest -> onShelf b rest

// Q1.2
let rec toShelf b bs =
    match bs with
    | [] -> [b]
    | b1::rest when b>=b1 -> b1::toShelf b rest
    | b1::rest -> b::b1::rest

// Q1.3
let rec fromShelf b bs =
    match bs with
    | [] -> None
    | b1::rest when b=b1 -> Some rest
    | b1::rest -> b1::fromShelf b rest

// let rec fromShelf b (bs: Shelf) =
//     match bs with
//     | [] -> None  // Book not found
//     | b1::rest when b = b1 -> Some rest  // Found the book, remove it
//     | b1::rest -> 
//         match fromShelf b rest with
//         | Some newShelf -> Some (b1 :: newShelf)  // Book found and removed in recursive call; add b1 back to front
//         | None -> None  // Book not found in recursive call

// Q1.4
let addLoan b n d ls =
    match ls with
    | [] -> [(b,n,d)]
    | _ -> (b,n,d)::ls

let rec removeLoan b n ls =
    match ls with
    | [] -> ls
    | (b1,n1,_)::rest when b1=b && n1=n -> rest
    | (b1,n1,_)::rest -> (b1,n1,_)::removeLoan b n rest

// Q1.5
let rec reminders d0 ls =
    match ls with
    | [] -> []
    | (b1,n1,d1)::rest when d1<d0 -> (b1,n1)::reminders d0 rest
    | (b1,n1,d1)::rest -> reminders d0 rest

// Q1.6
let rec toLetters nbls =
    match nbls with
    | [] -> []
    | (n,b)::rest -> ["\"Dear " + n + "!\n" + "Please return \"" + b + "\".\n" + "Regards Robin\""] @ toLetters rest

// Q1.7
//1.
let toLetters nbls =
    List.map (fun (n, b) -> "Dear " + n + "!\nPlease return \"" + b + "\".\nRegards, Robin") nbls

//2.
let reminders' d0 ls =
    List.foldBack (fun (b, n, d) acc -> 
        if d < d0 then (b, n) :: acc else acc) ls []

