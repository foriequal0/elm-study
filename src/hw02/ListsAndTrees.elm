module ListsAndTrees where

import List exposing ((::), append)
import List

suffixes : List a -> List (List a)
suffixes xs =
  case xs of
    h :: t -> xs :: suffixes t
    [] -> []

            
type Tree = Empty | Node Int Tree Tree

mem : Int -> Tree -> Bool
mem item tree =
  case tree of
    Empty ->
      False
      
    Node value left right ->
      (item == value) || (mem item left) || (mem item right)


fullTree : Int -> Int -> Tree
fullTree x h =
  if | h <= 0 -> Empty
     | otherwise ->
       let subTree = fullTree x (h-1)
       in Node x subTree subTree
         

balancedTree : Int -> Int -> Tree
balancedTree x n =
  if | n <= 0 -> Empty
     | n == 1 -> Node x Empty Empty
     | otherwise ->
       let m = n-1
       in
         if m % 2 == 0
         then let subTree = balancedTree x (m//2)
              in Node x subTree subTree
         else let (l, r) = create2 x (m//2)
              in Node x l r

create2 : Int -> Int -> (Tree, Tree)
create2 x m =
  (balancedTree x m, balancedTree x (m+1))

balancedTrees : Int -> Int -> List Tree
balancedTrees x n =
  if | n <= 0 -> [Empty]
     | n == 1 -> [Node x Empty Empty]
     | otherwise ->
       let m = n-1
           cartesian l1 l2 =
             List.concatMap (\a -> List.map (Node x a) l1) l2
       in
         if m % 2 == 0
         then let sub = balancedTrees x (m//2)
              in cartesian sub sub
         else let sub1 = balancedTrees x (m//2)
                  sub2 = balancedTrees x (m//2+1)
                  list1 = cartesian sub1 sub2
                  list2 = cartesian sub2 sub1
              in List.append list1 list2


completeTrees : Int -> Int -> List Tree
completeTrees x h =
  if | h <= 0 -> [Empty]
     | h == 1 -> [Node x Empty Empty]
     | otherwise ->
       let subTrees = completeTrees x (h-1)
           fullTree0 = fullTree x (h-2)
           fullTree1 = fullTree x (h-1)
           tree1 = List.map (\t -> Node x t fullTree0) subTrees
           tree2 = List.map (\t -> Node x fullTree1 t) subTrees
       in List.append tree1 tree2

almostCompleteTrees : Int -> Int -> List Tree
almostCompleteTrees x h =
  if | h <= 0 -> [Empty]
     | h == 1 -> [Node x Empty Empty]
     | otherwise ->
       let cartesian l1 l2 =
             List.concatMap (\a -> List.map (Node x a) l1) l2
           subTrees = almostCompleteTrees x (h-1)
           fullTree0 = fullTree x (h-2)
           fullTree1 = fullTree x (h-2)
           tree1 = List.map (\t -> Node x t fullTree0) subTrees
           tree2 = List.map (\t -> Node x fullTree1 t) subTrees
           tree3 = cartesian subTrees subTrees
       in List.concat [tree1, tree2, tree3]
