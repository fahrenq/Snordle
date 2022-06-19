module Renderer

open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open System

let cellToString (cell: Game.Cell) : string =
  match cell with
  | Game.Cell.Empty -> "S"
  | Game.Cell.Snake x -> x.ToString()

let fieldToText field =
  field
  |> Array.map (Array.map cellToString >> String.concat "")
  |> String.concat "\n"

let CELL_SIDE = 50.
let CELL_COLOR_1 = "#D3EBCD"
let CELL_COLOR_2 = "#AEDBCE"
let SNAKE_COLOR = "#635666"
let SNAKE_CELL_RATIO = 0.75
let SNAKE_STROKE_WIDTH = 2.

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
  (colIdx, rowIdx)
  =
  ctx.fillStyle <- !^SNAKE_COLOR

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
    drawSnakeCell ctx B (colIdx, rowIdx)
    drawSnakeCell ctx L (colIdx, rowIdx)

  | RB ->
    drawSnakeCell ctx R (colIdx, rowIdx)
    drawSnakeCell ctx B (colIdx, rowIdx)

  | LT ->
    drawSnakeCell ctx L (colIdx, rowIdx)
    drawSnakeCell ctx T (colIdx, rowIdx)

  | TR ->
    drawSnakeCell ctx T (colIdx, rowIdx)
    drawSnakeCell ctx R (colIdx, rowIdx)


let drawCell
  (ctx: CanvasRenderingContext2D)
  (surroundings: Game.Cell [] [])
  (colIdx, rowIdx, cell)
  =

  let calcLocation idx =
    (float idx * CELL_SIDE)
    + (CELL_SIDE - CELL_SIDE * SNAKE_CELL_RATIO) / 2. // add half of the empty space

  match cell with
  | Game.Cell.Snake x ->

    let snakePart =
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
      | [| _; [| Game.Snake a; Game.Snake cc; _ |]; _ |] when
        Math.Abs(cc - a) = 1
        ->
        L
      | [| _; [| _; Game.Snake cc; Game.Snake a |]; _ |] when
        Math.Abs(cc - a) = 1
        ->
        R
      | _ -> failwithf "drawCell (snake part): invalid surroundings"

    drawSnakeCell ctx snakePart (colIdx, rowIdx)

  | _ -> ()

let drawCanvas (canvas: HTMLCanvasElement) field : Game.Field =
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

    drawCell ctx surroundings (xIdx, yIdx, cell)
  )

  field
