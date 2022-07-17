[<RequireQualifiedAccess>]
module Game

open Queue
open Browser.Types

let writeLog = printf

type Cell =
  | Empty
  | Snake of int
  | Letter of char
  | Backspace

type Field = Cell [] []

let ALL_LETTERS = [| 'a' .. 'z' |]

let emptyField (width: int) (height: int) : Field =
  Array2D'.create width height Empty

type Direction =
  | Up
  | Down
  | Left
  | Right

let isOppositeDirections d1 d2 =
  d1 = Up && d2 = Down
  || d1 = Down && d2 = Up
  || d1 = Left && d2 = Right
  || d1 = Right && d2 = Left

let moveSnake
  (snakeLength: int)
  (direction: Direction)
  (field: Field)
  : Field * Cell * bool =
  let fieldCopy = field |> Array2D'.copy

  let headRowIdx, headColIdx =
    Array2D'.findIndexes (fun _ _ cell -> cell = (Snake 0)) fieldCopy
    |> Option.get

  let headTargetRowIdx, headTargetColIdx =
    fieldCopy
    |> Array2D'.wrapIndexes (
      match direction with
      | Up -> headRowIdx - 1, headColIdx
      | Right -> headRowIdx, headColIdx + 1
      | Down -> headRowIdx + 1, headColIdx
      | Left -> headRowIdx, headColIdx - 1
    )

  let headTargetCell = fieldCopy.[headTargetRowIdx].[headTargetColIdx]

  let canMove =
    match headTargetCell with
    | Empty -> true
    | Letter _ -> true
    | Backspace -> true
    | Snake 0 -> failwithf "Two heads? Wtf?"
    | Snake (_) -> false

  if canMove then
    Array2D'.iteri
      (fun rowIdx colIdx cell ->
        match cell with
        | Snake x ->
          let nextX = x + 1

          if nextX >= snakeLength then
            fieldCopy.[rowIdx].[colIdx] <- Empty
          else
            fieldCopy.[rowIdx].[colIdx] <- Snake nextX
        | _ -> ()
      )
      fieldCopy

    fieldCopy.[headTargetRowIdx].[headTargetColIdx] <- Snake 0

  fieldCopy, headTargetCell, canMove

let findNextDirection
  currentDirection
  currentLength
  (queue: Queue<Direction>)
  field
  =
  match queue.TryDequeue() with
  | None -> currentDirection
  | Some newDirection ->
    if isOppositeDirections currentDirection newDirection then
      currentDirection
    else
      newDirection


module Spawner =
  let spawnSnake snakeLength (field: Field) : Field =
    let fieldCopy = field |> Array2D'.copy

    let snake =
      [| 0 .. snakeLength - 1 |]
      |> Array.rev
      |> Array.map Snake

    fieldCopy.[0].[0 .. snakeLength - 1] <- snake
    fieldCopy

  let spawn (cell: Cell) rowIdx colIdx (field: Field) : Field =
    if field.[rowIdx].[colIdx] <> Empty then
      failwithf "Tried to spawn letter on non-empty cell"

    let fieldCopy = field |> Array2D'.copy
    fieldCopy.[rowIdx].[colIdx] <- cell
    fieldCopy

  let spawnRandomly (cells: Cell []) (field: Field) : Field =
    let fieldCopy = field |> Array2D'.copy

    let rng = System.Random()

    let emptyCellIndexesRnd =
      Array2D'.findAllIndexes (fun _ _ cell -> cell = Empty) fieldCopy
      |> Array.sortBy (fun _ -> rng.Next())

    let cellsPlacement =
      emptyCellIndexesRnd
      |> Array.take (Array.length cells)
      |> Array.zip cells

    cellsPlacement
    |> Array.fold
      (fun fieldState (c, (rowIdx, colIdx)) -> spawn c rowIdx colIdx fieldState)
      fieldCopy


  let spawnAllLettersRandomly = spawnRandomly (ALL_LETTERS |> Array.map Letter)
  let spawnBackspaceRandomly = spawnRandomly [| Backspace |]

let randomPossibleWord () =
  let rng = System.Random()
  Words.POSSIBLE_WORDS.[rng.Next Words.POSSIBLE_WORDS.Length]

type WordleMarker =
  | Grey
  | Yellow
  | Green

type WordleGraph = WordleMarker []

let calculateWordleGraph (word: string) (guess: string) =
  let guessCharacters =
    (guess.PadRight word.Length).ToCharArray()
    |> Array.map Some

  let wordCharacters = word.ToCharArray() |> Array.map Some

  let output = Array.create word.Length Grey

  // Find all green characters and delete them from the word and guess.
  wordCharacters
  |> Array.iteri (fun idx character ->
    if guessCharacters.[idx] = character then
      wordCharacters.[idx] <- None
      guessCharacters.[idx] <- None
      output.[idx] <- Green
  )

  // Find all yellow characters and delete them from the word and guess.
  wordCharacters
  |> Array.iteri (fun idx character ->
    match guessCharacters
          |> Array.tryFindIndex (fun c -> Option.isSome c && c = character)
      with
    | Some yellowIdx ->
      wordCharacters.[idx] <- None
      guessCharacters.[yellowIdx] <- None
      output.[yellowIdx] <- Yellow
    | None -> ()
  )

  output

type CheckGuessResult =
  | NotAllowed
  | NotGuessed of WordleGraph
  | Guessed

let checkGuess (word: string) (guess: string) =
  if guess = word then
    Guessed
  elif Words.ALLOWED_WORDS |> Array.contains guess then
    NotGuessed(calculateWordleGraph word guess)
  else
    NotAllowed

type GameResult =
  | Won
  | Died

type State =
  {
    Word: string
    BaseSnakeLength: int
    CurrentGuess: string
    PastGuesses: (string * CheckGuessResult) []
    KnownUnusedLetters: char []
    Direction: Direction
    CanvasEl: HTMLCanvasElement
    CurrentGuessEl: HTMLDivElement
    GuessHistoryEl: HTMLDivElement
    InputBuffer: Queue<Direction>
    Field: Field
    GameResult: GameResult option
  }

let tick state =
  let snakeLength = state.BaseSnakeLength + state.PastGuesses.Length

  let newDirection =
    findNextDirection state.Direction snakeLength state.InputBuffer state.Field

  let fieldAfterMove, headCell, moved =
    moveSnake snakeLength newDirection state.Field

  // Check what needs to be spawn
  let newField =
    match headCell with
    | Cell.Letter c -> Spawner.spawnRandomly [| Cell.Letter c |] fieldAfterMove
    | Cell.Backspace ->
      Spawner.spawnRandomly [| Cell.Backspace |] fieldAfterMove
    | _ -> fieldAfterMove

  let (newCurrentGuess, newPastGuesses) =
    match headCell with
    | Cell.Letter c ->
      let newCurrentGuess = state.CurrentGuess + c.ToString()
      writeLog $"New letter found. Current word: {newCurrentGuess}"

      if newCurrentGuess.Length = state.Word.Length then
        match checkGuess state.Word newCurrentGuess with
        | CheckGuessResult.Guessed as checkGuessResult ->
          writeLog "You win!"

          "",
          Array.append state.PastGuesses [| newCurrentGuess, checkGuessResult |]

        | CheckGuessResult.NotAllowed as checkGuessResult ->
          writeLog "Invalid word, get longer!"

          "",
          Array.append state.PastGuesses [| newCurrentGuess, checkGuessResult |]

        | CheckGuessResult.NotGuessed graph as checkGuessResult ->
          writeLog "Valid guess, but not the word!"

          "",
          Array.append state.PastGuesses [| newCurrentGuess, checkGuessResult |]

      else
        newCurrentGuess, state.PastGuesses
    | Cell.Backspace ->
      writeLog $"Backspace found. Current word: {state.CurrentGuess}"

      if state.CurrentGuess.Length = 0
         && state.PastGuesses.Length > 0 then
        writeLog "Backspace wraps around past guess"

        let lastGuess = Array.last state.PastGuesses |> fst

        writeLog $"Recovered last guess. Last guess: {lastGuess}"

        writeLog
          $"Setting current word to. Last guess: {lastGuess.Substring(0, lastGuess.Length - 1)}"


        lastGuess.Substring(0, lastGuess.Length - 1),
        Array.sub state.PastGuesses 0 (state.PastGuesses.Length - 1)
      else
        state.CurrentGuess.Substring(0, state.CurrentGuess.Length - 1),
        state.PastGuesses
    | _ -> state.CurrentGuess, state.PastGuesses

  let newKnownUnusedLetters =
    if newPastGuesses.Length <> state.PastGuesses.Length then
      let lastGuess = newPastGuesses |> Array.tryLast

      match lastGuess with
      | Some (lastWord, CheckGuessResult.NotGuessed graph) ->
        let unusedLetters =
          graph
          |> Array.indexed
          |> Array.filter (fun (_, marker) -> marker = WordleMarker.Grey)
          |> Array.map (fun (idx, _) -> lastWord.[idx])

        Array.append state.KnownUnusedLetters unusedLetters
        |> Array.distinct
      | _ -> state.KnownUnusedLetters
    else
      state.KnownUnusedLetters

  let newGameResult =
    if newPastGuesses.Length <> state.PastGuesses.Length then
      let lastCheckGuessResult =
        newPastGuesses |> Array.tryLast |> Option.map snd

      if lastCheckGuessResult = Some CheckGuessResult.Guessed then
        Some Won
      else
        None
    else if not moved then
      Some Died
    else
      None

  { state with
      CurrentGuess = newCurrentGuess
      PastGuesses = newPastGuesses
      Direction = newDirection
      KnownUnusedLetters = newKnownUnusedLetters
      Field = newField
      GameResult = newGameResult
  }
