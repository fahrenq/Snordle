module App

open Game
open Browser.Dom
open Queue

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

let inputBuffer = new Queue<Direction>(3)

document.onkeydown <-
  fun e ->
    match e.code with
    | "ArrowUp" -> inputBuffer.TryEnqueue Up |> ignore
    | "ArrowRight" -> inputBuffer.TryEnqueue Right |> ignore
    | "ArrowDown" -> inputBuffer.TryEnqueue Down |> ignore
    | "ArrowLeft" -> inputBuffer.TryEnqueue Left |> ignore
    | _ -> ()


let rec findNextDirection currentDirection (queue: Queue<Direction>) =
  match queue.TryDequeue() with
  | None -> currentDirection
  // | Some newDirection when currentDirection = newDirection ->
  //   findNextDirection currentDirection queue
  | Some newDirection ->
    let _, _, canMove = moveSnake field 5 newDirection

    if canMove then
      newDirection
    else
      findNextDirection currentDirection queue

let rec loop (prevDirection: Direction) =
  async {
    do! Async.Sleep 300
    let nextDirection = findNextDirection prevDirection inputBuffer

    writeLog ("Next direction: " + nextDirection.ToString())

    let newField, headCell, moved = moveSnake field 5 nextDirection
    field <- newField
    render ()
    return! loop nextDirection
  }

loop Right |> Async.Start
