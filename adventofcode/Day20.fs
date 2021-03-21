module Day20

let devisors n =
    let upper = System.Math.Sqrt(float n) |> int

    seq {
        for i in 1 .. upper do
            if (n % i) = 0 then
                if n / i = i then
                    yield i
                else
                    yield i
                    yield n / i
    }
    |> Seq.toList

let calc n =
    devisors n |> Seq.sumBy (fun s -> s * 10)

let find calcF threshold =
    let rec helper n =
        let presents = calcF n

        if presents >= threshold then
            n
        else
            helper (n + 1)

    helper 1

let day20 () =
    let input = 36000000
    find calc input

let calcPart2 n =
    devisors n
    |> Seq.filter (fun x -> x * 50 >= n)
    |> Seq.sumBy (fun s -> s * 11)

let day20Part2 () =
    let input = 36_000_000
    find calcPart2 input
