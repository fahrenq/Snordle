module App

open Game
open Browser.Dom

let mutable field = emptyField 15 15
field <- spawnSnake field 5

let canvas =
  document.querySelector ("#game canvas") :?> Browser.Types.HTMLCanvasElement

let textElement =
  document.querySelector ("#game #text") :?> Browser.Types.HTMLElement

let render () =
  Renderer.drawCanvas canvas field
  textElement.innerHTML <- Renderer.fieldToText field

render ()

// let rec loop () =
//   async {
//     field <- moveSnake field Right
//     render ()
//     do! Async.Sleep 100
//     return! loop ()
//   }

// loop () |> Async.Start


document.onkeydown <-
  fun e ->
    match e.code with
    | "ArrowUp" -> field <- moveSnake field 5 Up
    | "ArrowRight" -> field <- moveSnake field 5 Right
    | "ArrowDown" -> field <- moveSnake field 5 Down
    | "ArrowLeft" -> field <- moveSnake field 5 Left
    | _ -> ()

    render ()
