module Day20

let calc n =
    seq {
        for i in 1 .. n / 2 do
            if (n % i) = 0 then yield i
    }
    |> Seq.sum
    |> fun s -> (s + n) * 10

let find threshold =
    let rec helper n =
        if calc n >= threshold then
            n
        else
            helper (n + 1)

    helper (threshold / 100)

let day20 () =
    let input = 36_000_000
    find input
