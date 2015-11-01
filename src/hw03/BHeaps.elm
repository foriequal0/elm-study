module BHeaps
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

import List ((::))
import List

type Tree = Node Int (List Tree)
type alias Rank = Int
type alias InternalHeap = List (Rank, Tree)
type Heap = WrapHeap InternalHeap

{-- Internal Helpers ----------------------------------------------------}

-- TODO

{-- External Interface --------------------------------------------------}

empty : Heap
empty = WrapHeap []

isEmpty : Heap -> Bool
isEmpty h = h == empty

insert : Int -> Heap -> Heap
insert x h =
  -- TODO
  h

merge : Heap -> Heap -> Heap
merge h1 h2 =
  -- TODO
  h1

findMin : Heap -> Maybe Int
findMin h =
  -- TODO
  Nothing

deleteMin : Heap -> Maybe Heap
deleteMin h =
  -- TODO
  Nothing

