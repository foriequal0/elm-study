module Pi where

import Random
import Signal exposing ((<~))
import Time exposing (Time, fps)
import Text
import Graphics.Element exposing (Element, empty, show)
import Graphics.Collage exposing (..)
import Color exposing (Color, red, green)
import Random exposing (generate)
import String
import Window
  -- TODO: modify/add imports as needed


type Event = Repaint Time | Shoot Point

           
type alias Point =
  {
    x:Float,
    y:Float,
    hit: Bool,
    timestamp: Time
  }

                 
type alias State =
  {
    time: Time,
    hit: Int,
    total: Int,
    list: List Point
  }



pushAndPop : Int -> List a -> a -> List a
pushAndPop c list i = List.take c <| i :: list

                 
initState =
  {
    time = 0,
    hit = 0,
    total = 0,
    list = []
  }


fps = 60
genRate = 60
fadeout = 10
listLength = genRate * fadeout


view : (Int,Int) -> State -> Element
view (w,h) state =
  let max = min w h
      scale = toFloat(max)
  in collage max max
       <| List.map (pointElement scale state.time) state.list


setAlpha : Color -> Float -> Color
setAlpha c a =
  let rgb = Color.toRgb c
  in 
     Color.rgba rgb.red rgb.green rgb.blue a
          

pointElement : Float -> Time -> Point -> Form
pointElement scale time point =
  let
    ttl = Time.second * fadeout
    color = if point.hit then Color.red else Color.green
    alpha = clamp 0 1 <| (ttl - (time - point.timestamp)) / ttl
  in
    ngon 3 5
      |> filled (setAlpha color alpha)
      |> move (point.x * scale/2, point.y * scale/2)

       
genPoint : Time -> Random.Seed -> (Point, Random.Seed)
genPoint t s =
  let (rx, s1) = generate (Random.float -1.0 1.0) s
      (ry, s2) = generate (Random.float -1.0 1.0) s1
      hit = sqrt(rx^2 + ry^2) < 1
  in ( {x = rx, y = ry, hit = hit, timestamp = t}, s2)


signalRepaint : Signal Event
signalRepaint = Repaint << fst <~ (Time.timestamp <| Time.fps fps)


signalPointSeed : Signal (Point, Random.Seed)
signalPointSeed =
  let initial = genPoint 0 (Random.initialSeed 0)
  in
    Signal.foldp (\t st -> genPoint t (snd st)) initial <| fst <~ (Time.timestamp <| Time.fps genRate)
          

signalPoint : Signal Event
signalPoint =
    Shoot << fst <~ signalPointSeed

        
shootUpdate : Point -> State -> State
shootUpdate point state =
    if point.hit
    then
      {
        state |
        hit <- state.hit + 1,
        total <- state.total + 1,
        list <- pushAndPop listLength state.list point
      }
    else
      {
        state |
        total <- state.total + 1,
        list <- pushAndPop listLength state.list point
      }

    
fadeoutUpdate : Time -> State -> State
fadeoutUpdate time state =
  {
    state |
    time <- time
  }


upstate : Event -> State -> State
upstate event state =
  case event of
    Repaint t -> fadeoutUpdate t state
    Shoot p -> shootUpdate p state

               
main : Signal Element
main =
  Signal.map2 view Window.dimensions
    (Signal.foldp upstate initState <| Signal.merge signalPoint signalRepaint)
