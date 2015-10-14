module DrawTrees where

import List exposing ((::))
import List
import ListsAndTrees exposing (..)

import Color
import Signal exposing ((<~))
import Window
import Mouse
import Text as T
import Graphics.Element as E
import Graphics.Collage as C


foldh : (a -> state -> state) -> (state -> b) -> state -> Signal a -> Signal b
foldh update select init sig =
  let int = Signal.foldp update init sig
  in select <~ int

sampleListOn : Signal b -> List a -> Signal a
sampleListOn sig xs =
  let update _ st =
        case st of
          (_, [x]) -> (x, xs)
          (_, h::t) -> (h, t)
      init = case List.head xs of
               Just x -> (x, xs)
  in foldh update fst init sig

treeHeight : Tree -> Int
treeHeight t =
  case t of
    Empty -> 1
    Node _ lt rt ->
      let lh = treeHeight lt
          rh = treeHeight rt
      in (max lh rh) + 1


type alias ViewCtx =
  {
    x: Int,
    y: Int,
    w: Int,
    h: Int
  }

subView : ViewCtx -> Tree -> List C.Form
subView ctx t =
  let leftX = ctx.x - ctx.w
      rightX = ctx.x + ctx.w
      childY = ctx.y - ctx.h
      textToForm = C.text << T.fromString
      origin = (toFloat ctx.x, toFloat ctx.y)
  in
    case t of
      Empty -> [ C.circle 10.0
                 |> C.filled Color.white
                 |> C.move origin
               ,textToForm "E"
                 |> C.move origin ]
      Node x left right->
        let local = [ C.path [origin, (toFloat leftX, toFloat childY)]
                      |> C.traced C.defaultLine
                    , C.path [origin, (toFloat rightX, toFloat childY)]
                      |> C.traced C.defaultLine
                    ,C.circle 10.0
                      |> C.filled Color.white
                      |> C.move origin
                    , textToForm (toString x)
                      |> C.move origin ]
            leftCtx =
              { ctx |
                x <- leftX
              , y <- childY
              , w <- ctx.w // 2 }
            rightCtx =
              { ctx |
                x <- rightX
              , y <- childY
              , w <- ctx.w // 2 }
        in List.concat
             [ local
             , subView leftCtx left
             , subView rightCtx right]
            

view : (Int,Int) -> Tree -> E.Element
view (ww,wh) t =
  let h = treeHeight t
      initialCtx = {
        x = 0,
        y = wh//4,
        h = (wh//2) // h,
        w = ww // 4
      }
  in C.collage ww wh <| subView initialCtx t
  

signalTree : Signal Tree
signalTree =
  let trees = ListsAndTrees.completeTrees 0 8
  in sampleListOn Mouse.clicks trees

main : Signal E.Element
main =
  Signal.map2 view Window.dimensions signalTree
