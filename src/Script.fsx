#load "./Words.fs"
#load "./Array2d.fs"
#load "./Queue.fs"
#load "./Game.fs"


Game.randomPossibleWord ()




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

calculateWordleGraph "dddf" "addd"

let ``double yellow`` () =
  calculateWordleGraph "beady" "odder" = [| Grey; Yellow; Grey; Yellow; Grey |]

let ``double yellow 2`` () =
  calculateWordleGraph "aabb" "bbaa" = [| Yellow; Yellow; Yellow; Yellow |]

let ``no yellow if green`` () =
  calculateWordleGraph "beady" "daddy" = [| Grey; Yellow; Grey; Green; Green |]

let ``all grey`` () =
  calculateWordleGraph "abcd" "efgh" = [| Grey; Grey; Grey; Grey |]

let ``all green`` () =
  calculateWordleGraph "abcd" "abcd" = [| Green; Green; Green; Green |]

``double yellow`` ()
&& ``double yellow 2`` ()
&& ``no yellow if green`` ()
&& ``all grey`` ()
&& ``all green`` ()
