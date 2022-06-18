#load "./Array2d.fs"
#load "./Game.fs"

open Game

// let directionsBuffer: Direction [] = Array.zeroCreate 3
// printfn "%A" directionsBuffer

// directionsBuffer[0] = Direction.Up

// directionsBuffer |> Array.tryFindIndex isNull

let a = ResizeArray<Direction>()
a.Add Up
a.Add Down

a.RemoveAt 0

a[0]

a
