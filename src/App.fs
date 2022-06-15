module App

open Game
open Browser.Dom

let mutable field = emptyField 20 100
field <- spawnSnake field 8
let gameEl = document.querySelector ("#game") :?> Browser.Types.HTMLElement

let render () =
  gameEl.innerHTML <- Renderer.fieldToText field

render ()

let rec loop () =
  async {
    field <- moveSnake field Right
    render ()
    do! Async.Sleep 100
    return! loop ()
  }

loop () |> Async.Start


// document.onkeydown <-
//   fun e ->
//     match e.code with
//     | "ArrowUp" -> field <- moveSnake field Up
//     | "ArrowRight" -> field <- moveSnake field Right
//     | "ArrowDown" -> field <- moveSnake field Down
//     | "ArrowLeft" -> field <- moveSnake field Left
//     | _ -> ()

//     render ()
