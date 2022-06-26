module NodeListOf

open Browser.Types

let toArray (nodeListOf: NodeListOf<'t>) =
  Array.init nodeListOf.length (fun i -> nodeListOf.[i])
