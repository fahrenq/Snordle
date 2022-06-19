module App

open Browser.Dom
open Queue

// let LOG_EL = document.querySelector (".log") :?> Browser.Types.HTMLElement

// let writeLog text =
//   LOG_EL.innerHTML <- sprintf "%s\n%s" text LOG_EL.innerHTML

let INPUT_BUFFER = new Queue<Game.Direction>(3)

document.onkeydown <-
  fun e ->
    match e.code with
    | "ArrowUp" -> INPUT_BUFFER.TryEnqueue Game.Up |> ignore
    | "ArrowRight" -> INPUT_BUFFER.TryEnqueue Game.Right |> ignore
    | "ArrowDown" -> INPUT_BUFFER.TryEnqueue Game.Down |> ignore
    | "ArrowLeft" -> INPUT_BUFFER.TryEnqueue Game.Left |> ignore
    | _ -> ()


let CANVAS_EL =
  document.querySelector ("#game canvas") :?> Browser.Types.HTMLCanvasElement

let TEXT_EL =
  document.querySelector ("#game #text") :?> Browser.Types.HTMLElement

let rec gameLoop (direction: Game.Direction) length field =
  async {
    do! Async.Sleep 300

    let nextDirection = Game.findNextDirection direction length INPUT_BUFFER field

    let newField, headCell, moved = Game.moveSnake length nextDirection field
    Renderer.drawCanvas CANVAS_EL newField |> ignore

    return! gameLoop nextDirection length newField
  }

Game.emptyField 15 15
|> Game.spawnSnake 5
|> Renderer.drawCanvas CANVAS_EL
|> gameLoop Game.Right 5
|> Async.Start
