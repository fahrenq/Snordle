[<RequireQualifiedAccess>]
module Game

open Queue

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
    let _, _, canMove = moveSnake currentLength newDirection field

    if canMove then
      newDirection
    else
      currentDirection

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
