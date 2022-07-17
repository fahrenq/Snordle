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
  | Died

let initializeGameLoop
  (canvasEl: IRefValue<HTMLCanvasElement option>)
  (currentGuessEl: IRefValue<HTMLDivElement option>)
  (guessHistoryEl: IRefValue<HTMLDivElement option>)
  (initialState: State)
  =
  let rec gameLoop (state: State) =
    async {
      do! Async.Sleep 400

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
        printf "You died!"
        return GameResult.Died
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
            writeLog $"Backspace found. Current word: {state.CurrentGuess}"

            if state.CurrentGuess.Length = 0
               && state.PastGuesses.Length > 0 then
              writeLog "Backspace wraps around past guess"

              let lastGuess = Array.last state.PastGuesses |> fst

              writeLog $"Recovered last guess. Last guess: {lastGuess}"

              writeLog
                $"Setting current word to. Last guess: {lastGuess.Substring(0, lastGuess.Length - 1)}"


              lastGuess.Substring(0, lastGuess.Length - 1),
              Array.sub state.PastGuesses 0 (state.PastGuesses.Length - 1),
              state.KnownUnusedLetters,
              None
            else
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
          return Won
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
open Feliz.prop

[<ReactComponent>]
let InitialScreen (startGame) =
  Html.div
    [
      prop.className "game-screen-initial"
      prop.children
        [
          Html.div "It's like Wordle, but you're a snake!"
          Html.div "Use arrows or touch to move."
          Html.div "You can wrap around the edges."
          Html.button
            [
              prop.className "game-button"
              prop.text "Play"
              prop.onClick (fun _ -> startGame ())
            ]

        ]
    ]

[<ReactComponent>]
let WinScreen (startGame, word: string) =
  Html.div
    [
      prop.className "game-screen-win"
      prop.children
        [
          Html.div "You won!"
          Html.div "The word was:"
          Html.div
            [
              prop.className "guess guessed"
              prop.children (
                word.ToCharArray()
                |> Array.map (fun c ->
                  Html.span
                    [
                      prop.className "letter"
                      prop.text (c.ToString())
                    ]
                )
              )
            ]
          Html.button
            [
              prop.className "game-button"
              prop.text "Play again"
              prop.onClick (fun _ -> startGame ())
            ]
        ]
    ]

[<ReactComponent>]
let DeathScreen (startGame, word: string) =
  Html.div
    [
      prop.className "game-screen-death"
      prop.children
        [
          Html.div "You lost :("
          Html.div "The word was:"
          Html.div
            [
              prop.className "guess"
              prop.children (
                word.ToCharArray()
                |> Array.map (fun c ->
                  Html.span
                    [
                      prop.className "letter"
                      prop.text (c.ToString())
                    ]
                )
              )
            ]
          Html.button
            [
              prop.className "game-button"
              prop.text "Play again"
              prop.onClick (fun _ -> startGame ())
            ]
        ]
    ]

[<ReactComponent>]
let PlayScreen (setDeathScreen, setWinScreen) =
  let canvasRef = React.useRef (None)
  let currentGuessRef = React.useRef (None)
  let guessHistoryRef = React.useRef (None)

  React.useEffectOnce (fun () ->
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

    writeLog "Initializing game"
    writeLog $"The word is: {initialState.Word}"

    async.Bind(
      initializeGameLoop canvasRef currentGuessRef guessHistoryRef initialState,
      fun gameResult ->
        match gameResult with
        | GameResult.Won -> async.Return(setWinScreen initialState.Word)
        | GameResult.Died -> async.Return(setDeathScreen initialState.Word)
    )
    |> Async.Start
  )

  let (touchStartY, setTouchStartY) = React.useState<float option> (None)
  let (touchEndY, setTouchEndY) = React.useState<float option> (None)
  let (touchStartX, setTouchStartX) = React.useState<float option> (None)
  let (touchEndX, setTouchEndX) = React.useState<float option> (None)

  React.useEffect (
    (fun () ->
      match touchStartX, touchEndX, touchStartY, touchEndY with
      | Some startX, Some endX, Some startY, Some endY ->

        let xDiff = startX - endX
        let yDiff = startY - endY

        writeLog $"xDiff: {xDiff}, yDiff: {yDiff}"

        let direction =
          if abs xDiff > abs yDiff then // left-right
            if xDiff > 0. then
              Game.Left
            else
              Game.Right
          else if yDiff > 0. then
            Game.Up
          else
            Game.Down

        INPUT_BUFFER.TryEnqueue(direction) |> ignore
        setTouchStartY (None)
        setTouchEndY (None)
        setTouchStartX (None)
        setTouchEndX (None)
        ()
      | _ -> ()
    ),
    [| box touchEndY; box touchEndX |]
  )

  Html.div
    [
      prop.classes [ "game-screen-play" ]
      prop.onTouchStart (fun e ->
        setTouchStartX (Some e.touches.[0].clientX)
        setTouchStartY (Some e.touches.[0].clientY)
      )
      prop.onTouchMove (fun e ->
        setTouchEndX (Some e.touches.[0].clientX)
        setTouchEndY (Some e.touches.[0].clientY)
      )
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
              prop.children
                [
                  Html.h1 "Current Guess"
                  Html.div [ prop.ref currentGuessRef ]
                  Html.h1 "Past Guesses"
                  Html.div [ prop.ref guessHistoryRef ]
                ]
            ]

        ]
    ]

[<ReactComponent>]
let Game () =
  let (screen, setScreen) = React.useState (MenuScreen.Initial)

  Html.div
    [
      prop.className "game-root"
      prop.children
        [
          // Header
          Html.div
            [
              prop.className "game-header"
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

ReactDOM.render (Game(), document.getElementById "game")
