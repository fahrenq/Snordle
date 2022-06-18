module App

open Game
open Browser.Dom
open Queue
open System.Collections.Generic

let mutable field = emptyField 15 15
field <- spawnSnake field 5

let canvas =
  document.querySelector ("#game canvas") :?> Browser.Types.HTMLCanvasElement

let textElement =
  document.querySelector ("#game #text") :?> Browser.Types.HTMLElement

let log = document.querySelector (".log") :?> Browser.Types.HTMLElement

let writeLog text =
  log.innerHTML <- sprintf "%s\n%s" text log.innerHTML

let render () =
  Renderer.drawCanvas canvas field
  textElement.innerHTML <- Renderer.fieldToText field

render ()

let controls = ResizeArray<Direction>()

// document.onkeydown <-
//   fun e ->
//     match e.code with
//     | "ArrowUp" -> inputBuffer.TryEnqueue Up |> ignore
//     | "ArrowRight" -> inputBuffer.TryEnqueue Right |> ignore
//     | "ArrowDown" -> inputBuffer.TryEnqueue Down |> ignore
//     | "ArrowLeft" -> inputBuffer.TryEnqueue Left |> ignore
//     | _ -> ()

document.onkeydown <-
  fun e ->
    match e.code with
    | "ArrowUp" -> controls.Add Up
    | "ArrowRight" -> controls.Add Right
    | "ArrowDown" -> controls.Add Down
    | "ArrowLeft" -> controls.Add Left
    | _ -> ()

document.onkeyup <-
  fun e ->
    let r = controls.Remove Down
    writeLog (sprintf "KeyUp: %s, remove: %A" e.code r)
    match e.code with
    | "ArrowUp" -> controls.Remove Up |> ignore
    | "ArrowRight" -> controls.Remove Right |> ignore
    | "ArrowDown" -> controls.Remove Down |> ignore
    | "ArrowLeft" -> controls.Remove Left |> ignore
    | _ -> ()

let inputBuffer = new Queue<Direction>(3)

// let rec findNextDirection currentDirection (queue: Queue<Direction>) =
//   match queue.TryDequeue() with
//   | None -> currentDirection
//   | Some newDirection when currentDirection = newDirection ->
//     findNextDirection currentDirection queue
//   | Some newDirection ->
//     let _, _, canMove = moveSnake field 5 newDirection

//     if canMove then
//       newDirection
//     else
//       findNextDirection currentDirection queue

let findNextDirection currentDirection (controls: ResizeArray<Direction>) =
  if controls.Count = 0 then
    currentDirection
  else
    let newDirection = controls.[0]
    let _, _, canMove = moveSnake field 5 newDirection
    newDirection

    // if canMove then
    // else
    //   findNextDirection currentDirection queue

let rec loop (prevDirection: Direction) =
  async {
    do! Async.Sleep 300
    writeLog <| controls.ToString()
    let nextDirection = findNextDirection prevDirection controls

    writeLog ("Next direction: " + nextDirection.ToString())

    let newField, headCell, moved = moveSnake field 5 nextDirection
    field <- newField
    render ()
    return! loop nextDirection
  }

loop Right |> Async.Start
