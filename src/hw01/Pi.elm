module Pi where

import Random
import Signal exposing ((<~))
import Time
import Text
import Graphics.Element exposing (Element, empty, show)
import Graphics.Collage exposing (..)
import Color exposing (Color, red, green)
import Random exposing (generate)
import String
import Window
  -- TODO: modify/add imports as needed

type alias Point = { x:Float, y:Float }

type alias State = ((Int, List Point), (Int, List Point))

initState = ((0,[]), (0,[]))

upstate : Point -> State -> State
upstate pt st =
  let r = sqrt (pt.x^2 + pt.y^2)
  in
    if | r <= 1 ->
         let ((count, list), right) = st
         in ((count + 1, pt::list), right)
       | otherwise ->
         let (left, (count, list)) = st
         in (left, (count + 1, pt::list))

toText : a -> Text.Text
toText = Text.fromString << toString

view : (Int,Int) -> State -> Element
view (w,h) st =
  let m = min w h
  in collage m m
            <| List.map (pointElement (toFloat m) red) (st |> fst |> snd)
                 `List.append` List.map (pointElement (toFloat m) green) (st |> snd |> snd)
                 `List.append` [text <| toText ((st |> fst |> fst |> toFloat)/(st |> snd |> fst |> toFloat))]


pointElement : Float -> Color -> Point -> Form
pointElement s c p =
  move (p.x * s/2, p.y * s/2) <| filled c <| circle 5

       
genPoint : Random.Seed -> (Point, Random.Seed)
genPoint s =
  let (rx, s1) = generate (Random.float -1.0 1.0) s
      (ry, s2) = generate (Random.float -1.0 1.0) s1
  in ({x=rx,y=ry}, s2)

signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  Signal.foldp (\_ st -> genPoint (snd st)) (genPoint (Random.initialSeed 0)) (Time.fps 500)
        
signalPoint : Signal Point
signalPoint =
  fst <~ signalPointSeed

main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState signalPoint)
