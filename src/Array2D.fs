module Array2D'

let create (lengthRows: int) (lengthCols: int) valueY =
  Array.create lengthRows (Array.create lengthCols valueY)

let copy (aa: 'a [] []) : 'a [] [] =
  let lengthRows = Array.length aa
  let lengthCols = Array.length (aa.[0])

  Array.init
    lengthRows
    (fun rowIdx -> Array.init lengthCols (fun colIdx -> aa.[rowIdx].[colIdx]))

/// Returns first x,y index of the given value in the matrix.
let findIndexes f (aa: 'a [] []) =
  aa
  |> Array.indexed
  |> Array.tryPick (fun (rowIdx, rows) ->
    rows
    |> Array.indexed
    |> Array.tryPick (fun (colIdx, col) ->
      if f rowIdx colIdx col then
        Some(rowIdx, colIdx)
      else
        None
    )
  )

/// Returns all x,y index of the given value in the matrix.
let findAllIndexes f (aa: 'a [] []) =
  aa
  |> Array.indexed
  |> Array.map (fun (rowIdx, rows) ->
    rows
    |> Array.indexed
    |> Array.choose (fun (colIdx, col) ->
      if f rowIdx colIdx col then
        Some(rowIdx, colIdx)
      else
        None
    )
  )
  |> Array.collect id

let wrapIndexes (rowIdx, colIdx) (aa: 'a [] []) =
  let lengthRows = aa.Length
  let lengthCols = aa.[0].Length

  ((rowIdx % lengthRows) + lengthRows) % lengthRows,
  ((colIdx % lengthCols) + lengthCols) % lengthCols

let iteri f =
  Array.iteri (fun rowIdx rows ->
    rows
    |> Array.iteri (fun colIdx col -> f rowIdx colIdx col)
  )

let mapi f =
  Array.mapi (fun rowIdx rows ->
    rows
    |> Array.mapi (fun colIdx col -> f rowIdx colIdx col)
  )
