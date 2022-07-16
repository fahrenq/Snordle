module App

open Browser.Dom
open Queue
open Fable.React
open Browser.Types

let LOG_EL = document.querySelector (".log") :?> Browser.Types.HTMLElement

let writeLog text = printf text
// LOG_EL.innerHTML <- sprintf "%s\n%s" text LOG_EL.innerHTML

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


// let CANVAS_EL =
//   document.querySelector ("#game canvas") :?> Browser.Types.HTMLCanvasElement

// let TEXT_EL =
//   document.querySelector ("#game #text") :?> Browser.Types.HTMLElement

// let CURRENT_GUESS_EL =
//   document.querySelector ("#game #current-guess") :?> Browser.Types.HTMLElement

// let GUESS_HISTORY_EL =
//   document.querySelector ("#game #guess-history") :?> Browser.Types.HTMLElement

type State =
  {
    Word: string
    BaseSnakeLength: int
    CurrentGuess: string
    PastGuesses: (string * Game.CheckGuessResult) []
    KnownUnusedLetters: char []
    Direction: Game.Direction
    Field: Game.Field
  }

type GameResult =
  | Won
  | Lost

let initializeGameLoop
  (canvasEl: IRefValue<HTMLCanvasElement option>)
  (currentGuessEl: IRefValue<HTMLDivElement option>)
  (guessHistoryEl: IRefValue<HTMLDivElement option>)
  initialState
  =
  let rec gameLoop (state: State) =
    async {
      do! Async.Sleep 300

      let snakeLength = state.BaseSnakeLength + state.PastGuesses.Length

      let newDirection =
        Game.findNextDirection
          state.Direction
          snakeLength
          INPUT_BUFFER
          state.Field

      let fieldAfterMove, headCell, moved =
        Game.moveSnake snakeLength newDirection state.Field

      if not moved then
        printf "You lost!"
        return GameResult.Lost
      else
        let newField =
          match headCell with
          | Game.Cell.Letter c ->
            Game.Spawner.spawnRandomly [| Game.Cell.Letter c |] fieldAfterMove
          | Game.Cell.Backspace ->
            Game.Spawner.spawnRandomly [| Game.Cell.Backspace |] fieldAfterMove
          | _ -> fieldAfterMove

        let (newCurrentGuess,
             newPastGuesses,
             newKnownUnusedLetters,
             (gameResult: GameResult option)) =
          match headCell with
          | Game.Cell.Letter c ->
            let newCurrentGuess = state.CurrentGuess + c.ToString()

            writeLog $"New letter found. Current word: {newCurrentGuess}"

            if newCurrentGuess.Length = state.Word.Length then
              match Game.checkGuess state.Word newCurrentGuess with
              | Game.CheckGuessResult.Guessed as checkGuessResult ->
                writeLog "You win!"

                "",
                Array.append
                  state.PastGuesses
                  [| newCurrentGuess, checkGuessResult |],
                state.KnownUnusedLetters,
                Some Won

              | Game.CheckGuessResult.NotAllowed as checkGuessResult ->
                writeLog "Invalid word, get longer!"

                "",
                Array.append
                  state.PastGuesses
                  [| newCurrentGuess, checkGuessResult |],
                state.KnownUnusedLetters,
                None

              | Game.CheckGuessResult.NotGuessed graph as checkGuessResult ->
                writeLog "Valid guess, but not the word!"

                // update knownUnusedLetters
                let unusedLetters =
                  graph
                  |> Array.indexed
                  |> Array.filter (fun (_, marker) ->
                    marker = Game.WordleMarker.Grey
                  )
                  |> Array.map (fun (idx, _) -> newCurrentGuess.[idx])

                "",
                Array.append
                  state.PastGuesses
                  [| newCurrentGuess, checkGuessResult |],
                Array.append state.KnownUnusedLetters unusedLetters
                |> Array.distinct,
                None

            else
              newCurrentGuess, state.PastGuesses, state.KnownUnusedLetters, None
          | Game.Cell.Backspace ->
            writeLog "Backspace found. Current word: {state.CurrentGuess}"

            state.CurrentGuess.Substring(0, state.CurrentGuess.Length - 1),
            state.PastGuesses,
            state.KnownUnusedLetters,
            None
          | _ ->
            state.CurrentGuess,
            state.PastGuesses,
            state.KnownUnusedLetters,
            None

        if gameResult = Some Won then
          return GameResult.Won
        else
          canvasEl.current
          |> Option.iter (fun c ->
            Renderer.drawCanvas c state.KnownUnusedLetters newField
            |> ignore
          )

          currentGuessEl.current
          |> Option.iter (fun c ->
            Renderer.renderGuesses c [| newCurrentGuess |]
          )

          let pastGuessesToRender =
            newPastGuesses
            |> Array.rev
            |> Array.sortBy (fun (word, checkGuessResult) ->
              match checkGuessResult with
              | Game.CheckGuessResult.Guessed -> 0
              | Game.CheckGuessResult.NotGuessed _ -> 1
              | Game.CheckGuessResult.NotAllowed _ -> 2
            )

          guessHistoryEl.current
          |> Option.iter (fun c ->
            Renderer.renderGuesses c (pastGuessesToRender |> Array.map fst)
          )

          guessHistoryEl.current
          |> Option.iter (fun c ->
            Renderer.colorGuesses c (pastGuessesToRender |> Array.map snd)
          )

          let newState =
            { state with
                CurrentGuess = newCurrentGuess
                PastGuesses = newPastGuesses
                Direction = newDirection
                KnownUnusedLetters = newKnownUnusedLetters
                Field = newField
            }

          return! gameLoop newState
    }

  gameLoop initialState


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


// Renderer.drawCanvas CANVAS_EL initialState.KnownUnusedLetters initialState.Field
// |> ignore

// gameLoop initialState |> Async.Start

// Renderer.renderGuesses CURRENT_GUESS_EL [| "hello" |]

type MenuScreen =
  | Initial
  | Play
  | Death of string
  | Win of string


open Feliz

[<ReactComponent>]
let InitialScreen (startGame) =
  Html.div
    [
      Html.p "Use arrows to move"
      Html.button
        [
          prop.text "Play"
          prop.onClick (fun _ -> startGame ())
        ]
    ]

[<ReactComponent>]
let WinScreen (startGame, word: string) =
  Html.div
    [
      Html.p "You won!"
      Html.p $"The word was: {word}"
      Html.button
        [
          prop.text "Play again"
          prop.onClick (fun _ -> startGame ())
        ]
    ]

[<ReactComponent>]
let DeathScreen (startGame, word: string) =
  Html.div
    [
      Html.p "You lost :("
      Html.p $"The word was: {word}"
      Html.button
        [
          prop.text "Play again"
          prop.onClick (fun _ -> startGame ())
        ]
    ]

[<ReactComponent>]
let PlayScreen (setDeathScreen, setWinScreen) =
  let initialState =
    {
      Word = Game.randomPossibleWord ()
      Direction = Game.Right
      BaseSnakeLength = 5
      CurrentGuess = ""
      PastGuesses = [||]
      KnownUnusedLetters = [||]
      Field =
        (Game.emptyField 12 12
         |> Game.Spawner.spawnSnake BASE_SNAKE_LENGTH
         |> Game.Spawner.spawnAllLettersRandomly
         |> Game.Spawner.spawnBackspaceRandomly)
    }

  let canvasRef = React.useRef (None)
  let currentGuessRef = React.useRef (None)
  let guessHistoryRef = React.useRef (None)


  async.Bind(
    initializeGameLoop canvasRef currentGuessRef guessHistoryRef initialState,
    fun gameResult ->
      match gameResult with
      | GameResult.Won -> async.Return(setWinScreen (initialState.Word))
      | GameResult.Lost -> async.Return(setDeathScreen (initialState.Word))
  )
  |> Async.Start

  Html.div
    [
      prop.style [ style.display.flex ]
      prop.children
        [
          Html.canvas
            [
              prop.ref canvasRef
              prop.width 540
              prop.height 540
            ]
          Html.div
            [
              prop.className "guesses"
              prop.children [
                Html.div [ prop.ref currentGuessRef ]
                Html.div [ prop.ref guessHistoryRef ]
              ]
            ]

        ]
    ]

[<ReactComponent>]
let Game () =
  let (screen, setScreen) = React.useState (MenuScreen.Play)

  Html.div
    [
      prop.style
        [
          style.minHeight (length.px 590)
          style.backgroundColor "#90C8AC"
        ]
      prop.children
        [
          // Header
          Html.div
            [
              prop.style
                [
                  style.width (length.percent 100)
                  style.height (length.px 50)
                  style.backgroundColor "#90C8AC"
                  style.display.flex
                  style.justifyContent.center
                  style.alignItems.center
                  style.fontFamily "monospace"
                  style.fontSize (length.px 30)
                ]
              prop.children [ Html.span "Snordle" ]
            ]

          match screen with
          | Initial -> InitialScreen(fun _ -> setScreen MenuScreen.Play)
          | Win word -> WinScreen((fun _ -> setScreen MenuScreen.Play), word)
          | Death word ->
            DeathScreen((fun _ -> setScreen MenuScreen.Play), word)
          | Play ->
            PlayScreen(
              (fun word -> setScreen <| MenuScreen.Death word),
              (fun word -> setScreen <| MenuScreen.Win word)
            )

        ]

    ]

open Browser.Dom

ReactDOM.render (Game(), document.getElementById "game")
