module Renderer

open Game

let cellToString (cell: Cell) : string =
  match cell with
  | Empty -> " "
  | Snake (Head) -> "@"
  | Snake (Body) -> "."

let fieldToText field =
  field
  |> Array.map (Array.map cellToString >> String.concat "")
  |> String.concat "\n"
