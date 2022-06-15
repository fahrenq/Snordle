module Game

// type SnakePart =
//   | Head
//   | Body
//   | Tail

type Cell =
  | Empty
  | Snake of int

type Field = Cell [] []

let emptyField (width: int) (height: int) : Field =
  Array2D'.create width height Empty

let spawnSnake (field: Field) snakeLength : Field =
  let fieldCopy = field |> Array2D'.copy
  let snake = [| 0 .. snakeLength - 1 |] |> Array.map Snake
  fieldCopy.[0].[0 .. snakeLength - 1] <- snake
  fieldCopy

type Direction =
  | Up
  | Down
  | Left
  | Right

let moveSnake (field: Field) (snakeLength: int) (direction: Direction) : Field =
  let fieldCopy = field |> Array2D'.copy

  let headX, headY =
    Array2D'.findIndexes (fun _ _ cell -> cell = (Snake 0)) fieldCopy
    |> Option.get

  let headTargetX, headTargetY =
    fieldCopy
    |> Array2D'.wrapIndexes (
      match direction with
      | Up -> headX - 1, headY
      | Right -> headX, headY + 1
      | Down -> headX + 1, headY
      | Left -> headX, headY - 1
    )

  let headTargetCell = fieldCopy.[headTargetX].[headTargetY]

  let canMove =
    match headTargetCell with
    | Empty -> true
    | Snake 0 -> failwithf "Two heads? Wtf?"
    | Snake (_) -> false

  if not canMove then
    fieldCopy
  else
    Array2D'.iteri
      (fun xIdx yIdx cell ->
        match cell with
        | Snake x ->
          let nextX = x + 1

          if nextX >= snakeLength then
            fieldCopy.[xIdx].[yIdx] <- Empty
          else
            fieldCopy.[xIdx].[yIdx] <- Snake nextX
        | _ -> ()
      )
      fieldCopy

    fieldCopy.[headTargetX].[headTargetY] <- Snake 0

    fieldCopy
