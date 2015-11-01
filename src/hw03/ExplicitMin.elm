module ExplicitMin
  (Heap, empty, isEmpty, findMin, insert, deleteMin, merge) where

-- NOTE: without functors or type classes, we would manually swap in
-- different implementations of H by twiddling the following imports

import BinomialHeaps as H
-- import LeftistHeaps as H

type Heap =
       E
     | NE Int H.Heap   -- the Int is the minimum element

empty : Heap
empty = E

isEmpty : Heap -> Bool
isEmpty _ =
  -- TODO
  False

insert : Int -> Heap -> Heap
insert _ _ =
  -- TODO
  E

merge : Heap -> Heap -> Heap
merge _ _ =
  -- TODO
  E

findMin : Heap -> Maybe Int
findMin _ =
  -- TODO
  Nothing

deleteMin : Heap -> Maybe Heap
deleteMin _ =
  -- TODO
  Nothing

