// Problem 1
// Q1.1
let flip seqAB =
    Seq.map (fun (a, b) -> (b, a)) seqAB

//Q1.2
let dia n =
    seq {
        for i in 0 .. n do
            yield (i, n - i)
    }

// Q1.3
let allCoordinates =
    seq {
        for i in 0 .. 1000 do
        if i%2=1 then (flip (dia i)) else (dia i)
    }
    
