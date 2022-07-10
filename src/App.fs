module App

open Browser.Dom
open Queue

let LOG_EL = document.querySelector (".log") :?> Browser.Types.HTMLElement

let writeLog text =
  LOG_EL.innerHTML <- sprintf "%s\n%s" text LOG_EL.innerHTML

let INPUT_BUFFER = new Queue<Game.Direction>(3)
let BASE_SNAKE_LENGTH = 5

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

let CURRENT_GUESS_EL =
  document.querySelector ("#game #current-guess") :?> Browser.Types.HTMLElement

let GUESS_HISTORY_EL =
  document.querySelector ("#game #guess-history") :?> Browser.Types.HTMLElement

type State =
  {
    Word: string
    CurrentGuess: string
    PastGuesses: (string * Game.CheckGuessResult) []
    Direction: Game.Direction
    Field: Game.Field
  }

let rec gameLoop (state: State) =
  async {
    do! Async.Sleep 300

    let snakeLength = BASE_SNAKE_LENGTH + state.PastGuesses.Length

    let newDirection =
      Game.findNextDirection
        state.Direction
        snakeLength
        INPUT_BUFFER
        state.Field

    let fieldAfterMove, headCell, moved =
      Game.moveSnake snakeLength newDirection state.Field

    let newField =
      match headCell with
      | Game.Cell.Letter c ->
        Game.Spawner.spawnRandomly [| Game.Cell.Letter c |] fieldAfterMove
      | Game.Cell.Backspace ->
        Game.Spawner.spawnRandomly [| Game.Cell.Backspace |] fieldAfterMove
      | _ -> fieldAfterMove

    let newCurrentGuess, newPastGuesses =
      match headCell with
      | Game.Cell.Letter c ->
        let newCurrentGuess = state.CurrentGuess + c.ToString()

        writeLog $"New letter found. Current word: {newCurrentGuess}"

        if newCurrentGuess.Length = state.Word.Length then
          "",
          Array.append
            state.PastGuesses
            [|
              newCurrentGuess, Game.checkGuess state.Word newCurrentGuess
            |]
        else
          newCurrentGuess, state.PastGuesses
      | Game.Cell.Backspace ->
        writeLog "Backspace found. Current word: {state.CurrentGuess}"

        state.CurrentGuess.Substring(0, state.CurrentGuess.Length - 1),
        state.PastGuesses
      | _ -> state.CurrentGuess, state.PastGuesses

    Renderer.drawCanvas CANVAS_EL newField |> ignore
    Renderer.renderGuesses CURRENT_GUESS_EL [| newCurrentGuess |]

    let pastGuessesToRender =
      newPastGuesses
      |> Array.rev
      |> Array.sortBy (fun (word, checkGuessResult) ->
        match checkGuessResult with
        | Game.CheckGuessResult.Guessed -> 0
        | Game.CheckGuessResult.NotGuessed _ -> 1
        | Game.CheckGuessResult.NotAllowed _ -> 2
      )

    Renderer.renderGuesses
      GUESS_HISTORY_EL
      (pastGuessesToRender |> Array.map fst)

    Renderer.colorGuesses
      GUESS_HISTORY_EL
      (pastGuessesToRender |> Array.map snd)

    let newState =
      { state with
          CurrentGuess = newCurrentGuess
          PastGuesses = newPastGuesses
          Direction = newDirection
          Field = newField
      }

    return! gameLoop newState
  }

// let (field,_,_) =
//   Game.emptyField 15 15
//   |> Game.spawnSnake 5
//   |> Game.moveSnake 5 Game.Down

// let (field', _, _) =
//   field
//   |> Game.moveSnake 5 Game.Left
// // |> gameLoop Game.Right 5
// // |> Async.Start

// Renderer.drawCanvas CANVAS_EL field'

let initialState =
  {
    Word = Game.randomPossibleWord ()
    Direction = Game.Right
    CurrentGuess = ""
    PastGuesses = [||]
    Field =
      (Game.emptyField 12 12
       |> Game.Spawner.spawnSnake BASE_SNAKE_LENGTH
       |> Game.Spawner.spawnAllLettersRandomly
       |> Game.Spawner.spawnBackspaceRandomly)
  }

Renderer.drawCanvas CANVAS_EL initialState.Field
|> ignore

gameLoop initialState |> Async.Start

// Renderer.renderGuesses CURRENT_GUESS_EL [| "hello" |]
