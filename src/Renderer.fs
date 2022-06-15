module Renderer

open Game
open Browser.Types
open Fable.Core

let cellToString (cell: Cell) : string =
  match cell with
  | Empty -> "S"
  | Snake x -> x.ToString()

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
    ctx.fillStyle <- U3.Case1 CELL_COLOR_1
  else
    ctx.fillStyle <- U3.Case1 CELL_COLOR_2

  ctx.fillRect (
    float xIdx * CELL_SIDE,
    float yIdx * CELL_SIDE,
    CELL_SIDE,
    CELL_SIDE
  )

let drawCell
  (ctx: CanvasRenderingContext2D)
  (surroundings: Cell [] [])
  (xIdx, yIdx, cell)
  =

  let calcLocation idx =
    (float idx * CELL_SIDE)
    + (CELL_SIDE - CELL_SIDE * SNAKE_CELL_RATIO) / 2. // add half of the empty space

  match cell with
  | Snake x ->

    ctx.fillStyle <- U3.Case1 SNAKE_COLOR

    ctx.fillRect (
      calcLocation yIdx,
      calcLocation xIdx,
      CELL_SIDE * SNAKE_CELL_RATIO,
      CELL_SIDE * SNAKE_CELL_RATIO
    )

    ctx.fillStyle <- U3.Case1 "white"
    ctx.fillText (x.ToString(), calcLocation yIdx + 20., calcLocation xIdx + 20.)

  | _ -> ()

let drawCanvas (canvas: HTMLCanvasElement) field =
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
