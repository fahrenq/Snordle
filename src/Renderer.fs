module Renderer

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open System

let cellToString (cell: Game.Cell) : string =
  match cell with
  | Game.Cell.Empty -> "S"
  | Game.Cell.Snake x -> x.ToString()
  | Game.Cell.Letter x -> x.ToString()
  | Game.Cell.Backspace -> "🔙"

let fieldToText field =
  field
  |> Array.map (Array.map cellToString >> String.concat "")
  |> String.concat "\n"


let CELL_COLOR_1 = "#D3EBCD"
let CELL_COLOR_2 = "#AEDBCE"
let SNAKE_COLOR = "#635666"
let SNAKE_HEAD_COLOR = "#453B47"
let FONT = "'Open Sans', sans-serif"

let CELL_SIDE = 45.
let SNAKE_CELL_RATIO = 0.75
let SNAKE_STROKE_WIDTH = 2.
let TEXT_SIZE = 30.

let drawCellBackground (ctx: CanvasRenderingContext2D) (xIdx, yIdx, _) =
  if (yIdx + xIdx) % 2 = 0 then
    ctx.fillStyle <- !^CELL_COLOR_1
  else
    ctx.fillStyle <- !^CELL_COLOR_2

  ctx.fillRect (
    float xIdx * CELL_SIDE,
    float yIdx * CELL_SIDE,
    CELL_SIDE,
    CELL_SIDE
  )

type SnakeCell =
  | T
  | R
  | B
  | L
  | TB
  | LR
  | LT
  | TR
  | BL
  | RB

let rec drawSnakeCell
  (ctx: CanvasRenderingContext2D)
  (snakeCell: SnakeCell)
  (snakePart: int)
  (colIdx, rowIdx)
  =
  ctx.fillStyle <-
    if snakePart = 0 then
      !^SNAKE_HEAD_COLOR
    else
      !^SNAKE_COLOR

  // ctx.globalAlpha <- 1. - ((float snakePart) / 10.)

  match snakeCell with
  | T ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      y,
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE * SNAKE_CELL_RATIO
    )
  | B ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      y + (CELL_SIDE * (1. - SNAKE_CELL_RATIO)),
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE * SNAKE_CELL_RATIO
    )

  | L ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x,
      y + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE * SNAKE_CELL_RATIO
    )

  | R ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x + (CELL_SIDE * (1. - SNAKE_CELL_RATIO)),
      y + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE * SNAKE_CELL_RATIO
    )

  | LR ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x,
      y + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      CELL_SIDE,
      CELL_SIDE * SNAKE_CELL_RATIO
    )

  | TB ->
    let x = float rowIdx * CELL_SIDE
    let y = float colIdx * CELL_SIDE

    ctx.fillRect (
      x + (CELL_SIDE * (1. - SNAKE_CELL_RATIO) / 2.),
      y,
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE
    )

  | BL ->
    drawSnakeCell ctx B snakePart (colIdx, rowIdx)
    drawSnakeCell ctx L snakePart (colIdx, rowIdx)

  | RB ->
    drawSnakeCell ctx R snakePart (colIdx, rowIdx)
    drawSnakeCell ctx B snakePart (colIdx, rowIdx)

  | LT ->
    drawSnakeCell ctx L snakePart (colIdx, rowIdx)
    drawSnakeCell ctx T snakePart (colIdx, rowIdx)

  | TR ->
    drawSnakeCell ctx T snakePart (colIdx, rowIdx)
    drawSnakeCell ctx R snakePart (colIdx, rowIdx)

let private snakeCellFromSurroundings surroundings =
  match surroundings with
  | [| _; [| Game.Snake a; Game.Snake cc; _ |]; [| _; Game.Snake b; _ |] |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    BL
  | [| _; [| _; Game.Snake cc; Game.Snake a |]; [| _; Game.Snake b; _ |] |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    RB
  | [| [| _; Game.Snake a; _ |]; [| Game.Snake b; Game.Snake cc; _ |]; _ |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    LT
  | [| [| _; Game.Snake a; _ |]; [| _; Game.Snake cc; Game.Snake b |]; _ |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    TR
  | [| [| _; Game.Snake a; _ |]
       [| _; Game.Snake cc; _ |]
       [| _; Game.Snake b; _ |] |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    TB
  | [| _; [| Game.Snake a; Game.Snake cc; Game.Snake b |]; _ |] when
    Math.Abs(cc - a) = 1 && Math.Abs(cc - b) = 1
    ->
    LR
  | [| [| _; Game.Snake a; _ |]; [| _; Game.Snake cc; _ |]; [| _; _; _ |] |] when
    Math.Abs(cc - a) = 1
    ->
    T
  | [| _; [| _; Game.Snake cc; _ |]; [| _; Game.Snake a; _ |] |] when
    Math.Abs(cc - a) = 1
    ->
    B
  | [| _; [| Game.Snake a; Game.Snake cc; _ |]; _ |] when Math.Abs(cc - a) = 1 ->
    L
  | [| _; [| _; Game.Snake cc; Game.Snake a |]; _ |] when Math.Abs(cc - a) = 1 ->
    R
  | _ -> failwithf "drawCell (snake part): invalid surroundings"

let drawCell
  (ctx: CanvasRenderingContext2D)
  (knownUnusedLetters: char [])
  (surroundings: Game.Cell [] [])
  (colIdx, rowIdx, cell)
  =

  match cell with
  | Game.Cell.Snake snakePart ->
    let snakeCell = snakeCellFromSurroundings surroundings
    drawSnakeCell ctx snakeCell snakePart (colIdx, rowIdx)
  | Game.Cell.Letter letter ->
    let x = float rowIdx * CELL_SIDE + (CELL_SIDE / 2.)
    let y = float colIdx * CELL_SIDE + (CELL_SIDE / 2.)

    if Array.contains letter knownUnusedLetters then
      ctx.fillStyle <- !^ "grey"
    else
      ctx.fillStyle <- !^ "black"

    ctx.font <- $"{TEXT_SIZE}px {FONT}"
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "center"
    ctx.fillText (letter.ToString().ToUpper(), x, y)

  | Game.Cell.Backspace _ ->
    let x = float rowIdx * CELL_SIDE + (CELL_SIDE / 2.)
    let y = float colIdx * CELL_SIDE + (CELL_SIDE / 2.)

    ctx.fillStyle <- !^ "balck"
    ctx.font <- $"{TEXT_SIZE}px {FONT}"
    ctx.textBaseline <- "middle"
    ctx.textAlign <- "center"
    ctx.fillText ("🔙", x, y)
  | Game.Cell.Empty -> ()

let drawCanvas
  (canvas: HTMLCanvasElement)
  (knownUnusedLetters: char [])
  field
  : Game.Field =
  let ctx = canvas.getContext_2d None

  field
  |> Array2D'.iteri (fun xIdx yIdx cell ->
    drawCellBackground ctx (xIdx, yIdx, cell)
  )

  field
  |> Array2D'.iteri (fun xIdx yIdx cell ->
    let surroundings =
      [|
        [|
          xIdx - 1, yIdx - 1
          xIdx - 1, yIdx
          xIdx - 1, yIdx + 1
        |]
        [|
          xIdx, yIdx - 1
          xIdx, yIdx
          xIdx, yIdx + 1
        |]
        [|
          xIdx + 1, yIdx - 1
          xIdx + 1, yIdx
          xIdx + 1, yIdx + 1
        |]
      |]
      |> Array.map (
        Array.map (fun idxs ->
          let (xIdx, yIdx) = Array2D'.wrapIndexes idxs field
          field.[xIdx].[yIdx]
        )
      )

    drawCell ctx knownUnusedLetters surroundings (xIdx, yIdx, cell)
  )

  field

open Browser.Dom

let private fillGuess (el: HTMLElement) (guess: string) =
  el.querySelectorAll ".letter"
  |> NodeListOf.toArray
  |> Array.iteri (fun i el ->
    match guess.ToCharArray() |> Array.tryItem i with
    | Some letter -> el.innerHTML <- letter.ToString()
    | None -> ()
  )

let private emptyGuessHTML length =
  Array.create length """<span class="letter"></span>"""
  |> String.concat ""

let renderGuesses (el: HTMLElement) (guesses: string []) =
  let guessesHTML =
    guesses
    |> Array.map (fun guess ->
      let guessEl = document.createElement "div"
      guessEl.classList.add "guess"
      guessEl.innerHTML <- emptyGuessHTML 5
      fillGuess guessEl guess
      guessEl.outerHTML
    )
    |> String.concat ""

  el.innerHTML <- guessesHTML

let private colorGuess (el: Element) (wordleGraph: Game.WordleGraph) =
  el.querySelectorAll "span.letter"
  |> NodeListOf.toArray
  |> Array.iteri (fun i letterEl ->
    let wordleMarker = wordleGraph.[i]

    let htmlClass =
      match wordleMarker with
      | Game.WordleMarker.Green -> "green"
      | Game.WordleMarker.Grey -> "grey"
      | Game.WordleMarker.Yellow -> "yellow"

    letterEl.classList.add htmlClass
  )

let colorGuesses (el: HTMLElement) (guesses: Game.CheckGuessResult []) =
  el.querySelectorAll ".guess"
  |> NodeListOf.toArray
  |> Array.iteri (fun i guessEl ->
    let checkGuessResult = guesses.[i]

    match checkGuessResult with
    | Game.CheckGuessResult.NotGuessed wordleGraph ->
      colorGuess guessEl wordleGraph
      guessEl.classList.add "not-guessed"
    | Game.CheckGuessResult.NotAllowed -> guessEl.classList.add "not-allowed"
    | Game.CheckGuessResult.Guessed -> guessEl.classList.add "guessed"
  )

let renderState (prevState: Game.State) (newState: Game.State) (force: bool) =
  drawCanvas newState.CanvasEl newState.KnownUnusedLetters newState.Field
  |> ignore

  if force
     || (prevState.CurrentGuess <> newState.CurrentGuess) then
    renderGuesses newState.CurrentGuessEl [| newState.CurrentGuess |]

  if force
     || (prevState.PastGuesses <> newState.PastGuesses) then
    let pastGuessesToRender =
      newState.PastGuesses
      |> Array.rev
      |> Array.sortBy (fun (word, checkGuessResult) ->
        match checkGuessResult with
        | Game.CheckGuessResult.Guessed -> 0
        | Game.CheckGuessResult.NotGuessed _ -> 1
        | Game.CheckGuessResult.NotAllowed _ -> 2
      )

    renderGuesses newState.GuessHistoryEl (pastGuessesToRender |> Array.map fst)
    colorGuesses newState.GuessHistoryEl (pastGuessesToRender |> Array.map snd)
