module LHeaps where

import List exposing ((::))
import List

{-- Copied from LeftistHeaps.elm from class. DO NOT MODIFY. -------------}

type alias Rank = Int
type Heap = E | T Rank Int Heap Heap

rank : Heap -> Rank
rank h = case h of {E -> 0; T r _ _ _ -> r}

makeT : Int -> Heap -> Heap -> Heap
makeT x h1 h2 =
  let (l,r) = if | rank h1 >= rank h2 -> (h1, h2)
                 | otherwise          -> (h2, h1) in
  T (1 + rank r) x l r

merge : Heap -> Heap -> Heap
merge h1 h2 = case (h1, h2) of
  (_, E) -> h1
  (E, _) -> h2 
  (T _ x1 l1 r1, T _ x2 l2 r2) ->
    if | x1 <= x2  -> makeT x1 l1 (merge r1 h2)
       | otherwise -> makeT x2 l2 (merge h1 r2)

{------------------------------------------------------------------------}

 {-- slow. O(n) --}
insert : Int -> Heap -> Heap
insert x h =
  case h of
    E ->
      T 1 x E E
        
    T _ v l r ->
      let mn = min x v
          mx = max x v
      in makeT mn (insert mx l) r

mergePair : List Heap -> List Heap
mergePair l =
  case l of
    h1::h2::t -> merge h1 h2 :: mergePair t
    [x] -> [x]
    [] -> []

makePass : List Heap -> List Heap
makePass l =
  let len = List.length l in
  if | len > 1 -> makePass <| mergePair l
     | otherwise -> l
    
fromList : List Int -> Heap
fromList l =
  let h = List.head <| makePass <| List.map (\x -> makeT x E E) l
  in case h of
       Just x -> x

       
        
