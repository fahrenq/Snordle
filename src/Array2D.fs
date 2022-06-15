module Array2D'

let create (lengthX: int) (lengthY: int) valueY =
  Array.create lengthX (Array.create lengthY valueY)

let copy (xs: 'a [] []) : 'a [] [] =
  let lengthX = Array.length xs
  let lengthY = Array.length (xs.[0])

  Array.init lengthX (fun idX -> Array.init lengthY (fun idY -> xs.[idX].[idY]))

/// Returns first x,y index of the given value in the matrix.
let findIndexes f (xs: 'a [] []) =
  xs
  |> Array.indexed
  |> Array.tryPick (fun (idX, ys) ->
    ys
    |> Array.indexed
    |> Array.tryPick (fun (idY, y) ->
      if f idX idY y then
        Some(idX, idY)
      else
        None
    )
  )

let wrapIndexes (idX, idY) (xs: 'a [] []) =
  let lengthX = xs.Length
  let lengthY = xs.[0].Length
  ((idX % lengthX) + lengthX) % lengthX, ((idY % lengthY) + lengthY) % lengthY

let iteri f =
  Array.iteri (fun idX ys -> ys |> Array.iteri (fun idY y -> f idX idY y))

let mapi f =
  Array.mapi (fun idX ys -> ys |> Array.mapi (fun idY y -> f idX idY y))
