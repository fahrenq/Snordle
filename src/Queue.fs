module Queue

type Queue<'T>(size) =
  let queue: ResizeArray<'T> = ResizeArray<'T>()

  member this.TryEnqueue(i: 'T) =
    if queue.Count + 1 > size then
      false
    else
      queue.Add i
      true

  member this.TryDequeue() =
    if queue.Count > 0 then
      let r = queue.[0]
      queue.RemoveAt 0
      Some r
    else
      None
