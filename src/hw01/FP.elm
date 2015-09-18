module FP where


import List exposing ((::), append)  -- TODO: modify imports if you'd like
import List
import Result exposing (..)


digitsOfInt : Int -> List Int
digitsOfInt n =
  if | n == 0 -> []
     | True   -> digitsOfInt (n // 10) `append` [ (n % 10) ]


additivePersistence : Int -> Int
additivePersistence n =
  let sumDigits  = List.sum <| digitsOfInt <| n
  in
    if | sumDigits >= 10 -> 1 + additivePersistence sumDigits
       | True            -> 1


digitalRoot : Int -> Int
digitalRoot n =
  let sumDigits = List.sum <| digitsOfInt <| n
  in
    if | sumDigits >= 10 -> digitalRoot sumDigits
       | True -> sumDigits


subsequences : List a -> List (List a)
subsequences xs =
  case xs of
    h :: t ->
      subsequences t `append` List.map ((::) h) (subsequences t)

    [] ->
      [[]]


take : Int -> List a -> Result String (List a)
take k xs =
  if | k < 0 -> Err "negative"
     | k == 0 -> Ok []
     | otherwise ->
       case xs of
         hd :: tl ->
           take (k-1) tl `andThen` \x -> Ok(hd::x)

         [] ->
           Err "small"
