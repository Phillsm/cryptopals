module MyCollectionLib

//let private splitEvery (n : int) s =
//    seq { let r = ResizeArray<_>(n)
//          for x in s do
//              r.Add(x)
//              if r.Count = n then
//                  yield r.ToArray() |> Seq.ofArray
//                  r.Clear()
//          if r.Count <> 0 then
//              yield r.ToArray() |> Seq.ofArray }

type System.Array with 

    member this.chunksOf (n : int) = 
       [| let r = ResizeArray<_>(n)
          for x in this do
              r.Add(x)
              if r.Count = n then
                  yield r.ToArray()
                  r.Clear()
          if r.Count <> 0 then
              yield r.ToArray()  |]


module Array = 

    let chunksOf ( n : int) (b : 'a[]) =
        [| let r = ResizeArray<'a>(n)
           for x in b do
               r.Add(x)
               if r.Count = n then
                   yield r.ToArray()
                   r.Clear()
           if r.Count <> 0 then
               yield r.ToArray()  |]


type System.Collections.IEnumerable with 
    member this.splitEvery (n : int) = 
       seq { let r = ResizeArray<_>(n)
             for x in this do
                 r.Add(x)
                 if r.Count = n then
                      yield r.ToArray() |> Seq.ofArray
                      r.Clear()
             if r.Count <> 0 then
                  yield r.ToArray() |> Seq.ofArray } 

module Seq = 
    let chunksOf (n : int) (sequence : seq<'a>) = 
        sequence.splitEvery n

type List<'a> with 
    member this.chunksOf (n : int) = 
        [| let r = ResizeArray<_>(n)
           for x in this do
               r.Add(x)
               if r.Count = n then
                   yield r.ToArray() |> List.ofArray
                   r.Clear()
           if r.Count <> 0 then
               yield r.ToArray() |> List.ofArray |]
 
module List = 
    let chunksOf (n : int) (list : list<'a>) =
        list.chunksOf n
       