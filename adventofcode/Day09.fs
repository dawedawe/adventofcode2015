module Day09

open System.Collections.Generic
open System.Linq

[<Literal>]
let InputFile = "Day09Input.txt"

type Edge = { A: string; B: string; Dist: int }

let parse (s: string) =
    let words = s.Split(' ')

    [| { A = words.[0]
         B = words.[2]
         Dist = int words.[4] }
       { A = words.[2]
         B = words.[0]
         Dist = int words.[4] } |]

let getVertices (edges: Edge []) =
    edges
    |> Array.map (fun e -> Set.ofList [ e.A; e.B ])
    |> Set.unionMany

let countVertices (edges: Edge []) = edges |> getVertices |> Set.count

let isPossibleNextEdge (routeCandidate: Edge []) edge =
    let vertices = getVertices routeCandidate

    Array.last routeCandidate
    |> fun c -> c.B = edge.A && not (vertices.Contains(edge.B))

let bfs (edges: Edge []) =
    let maxLength = countVertices edges - 1
    let queue = Queue<Edge []>()
    let completedRoutes = ResizeArray<Edge []>()

    for e in edges do
        queue.Enqueue(Array.singleton e)

    while (queue.Count <> 0) do
        let routeCandidate = queue.Dequeue()

        if routeCandidate.Length = maxLength then
            completedRoutes.Add(routeCandidate)
        else
            let possibleNextEdges =
                edges
                |> Array.filter (isPossibleNextEdge routeCandidate)

            for e in possibleNextEdges do
                let routeCandidate' =
                    Array.append routeCandidate (Array.singleton e)

                queue.Enqueue(routeCandidate')

    completedRoutes

let calcDist (route: Edge []) = route |> Array.sumBy (fun e -> e.Dist)

let day09 () =
    let edges =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.collect id

    let routes = bfs edges
    let minDist = routes.Select(calcDist).Min()
    minDist

let day09Part2 () =
    let edges =
        InputFile
        |> System.IO.File.ReadAllLines
        |> Array.map parse
        |> Array.collect id

    let routes = bfs edges
    let maxDist = routes.Select(calcDist).Max()
    maxDist
