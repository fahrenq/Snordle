module App

open Browser.Dom
open Queue
open Feliz

let writeLog = printf

let BASE_SNAKE_LENGTH = 5

let rec gameLoop (prevState: Game.State) =
  async {
    do! Async.Sleep 400
    let newState = Game.tick prevState
    Renderer.renderState prevState newState false

    match newState.GameResult with
    | Some Game.GameResult.Won -> return Game.GameResult.Won
    | Some Game.GameResult.Died -> return Game.GameResult.Died
    | _ -> return! gameLoop newState

  }

type MenuScreen =
  | Initial
  | Play
  | Death of string
  | Win of string


[<ReactComponent>]
let InitialScreen (startGame) =
  document.onkeydown <- (fun e -> if e.key = " " then startGame ())

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
  document.onkeydown <- (fun e -> if e.key = " " then startGame ())

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
  document.onkeydown <- (fun e -> if e.key = " " then startGame ())

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
  let inputBuffer = React.useRef (new Queue<Game.Direction>(3))

  React.useEffectOnce (fun () ->
    let initialState: Game.State =
      {
        Word = Game.randomPossibleWord ()
        Direction = Game.Right
        BaseSnakeLength = 5
        CurrentGuess = ""
        PastGuesses = [||]
        KnownUnusedLetters = [||]
        CanvasEl = canvasRef.current.Value
        CurrentGuessEl = currentGuessRef.current.Value
        GuessHistoryEl = guessHistoryRef.current.Value
        GameResult = None
        InputBuffer = inputBuffer.current
        Field =
          (Game.emptyField 12 12
           |> Game.Spawner.spawnSnake BASE_SNAKE_LENGTH
           |> Game.Spawner.spawnAllLettersRandomly
           |> Game.Spawner.spawnBackspaceRandomly)
      }

    writeLog "Initializing game"
    writeLog $"The word is: {initialState.Word}"

    document.onkeydown <-
      printf "Registering onkeydown"

      fun e ->
        match e.code with
        | "ArrowUp" -> inputBuffer.current.TryEnqueue Game.Up |> ignore
        | "ArrowRight" ->
          inputBuffer.current.TryEnqueue Game.Right
          |> ignore
        | "ArrowDown" -> inputBuffer.current.TryEnqueue Game.Down |> ignore
        | "ArrowLeft" -> inputBuffer.current.TryEnqueue Game.Left |> ignore
        | _ -> ()

    async {
      Renderer.renderState initialState initialState true
      let! gameResult = gameLoop initialState

      match gameResult with
      | Game.Won -> setWinScreen initialState.Word
      | Game.Died -> setDeathScreen initialState.Word

    }
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

        inputBuffer.current.TryEnqueue(direction)
        |> ignore

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
