module Game

type SnakePart =
  | Head
  | Body

type Cell =
  | Empty
  | Snake of SnakePart

type Field = Cell [] []

let emptyField (width: int) (height: int) : Field =
  Array2D'.create width height Empty

let spawnSnake (field: Field) snakeLength : Field =
  let fieldCopy = field |> Array2D'.copy

  let snake =
    [|
      for i in [ 0 .. snakeLength - 1 ] ->
        if i = snakeLength - 1 then
          Head
        else
          Body
    |]
    |> Array.map Snake

  fieldCopy.[0].[0 .. snakeLength - 1] <- snake
  fieldCopy

type Direction =
  | Up
  | Down
  | Left
  | Right

let moveSnake (field: Field) (direction: Direction) : Field =
  let fieldCopy = field |> Array2D'.copy

  let headX, headY =
    Array2D'.findIndexes (fun _ _ cell -> cell = (Snake Head)) fieldCopy
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
    | Snake (Body _) -> false
    | Snake (Head _) -> failwithf "Two heads? Wtf?"

  if not canMove then
    fieldCopy
  else
    let tailX, tailY =
      let tailPredicate x y cell =
        let isBody = cell = Snake Body

        let isOneNeighbour =
          let snakeNeighbours =
            [|
              x - 1, y
              x + 1, y
              x, y - 1
              x, y + 1
            |]
            |> Array.map (fun idxs -> Array2D'.wrapIndexes idxs fieldCopy)
            |> Array.map (fun (x, y) -> fieldCopy.[x].[y])
            |> Array.filter (fun cell ->
              match cell with
              | (Snake _) -> true
              | _ -> false
            )

          snakeNeighbours.Length = 1

        isBody && isOneNeighbour

      Array2D'.findIndexes tailPredicate fieldCopy
      |> Option.get

    fieldCopy.[tailX].[tailY] <- Empty
    fieldCopy.[headX].[headY] <- Snake Body
    fieldCopy.[headTargetX].[headTargetY] <- Snake Head

    fieldCopy
