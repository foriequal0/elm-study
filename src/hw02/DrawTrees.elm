module DrawTrees where

import List ((::))
import List
import ListsAndTrees (..)

import Color
import Signal
import Window
import Mouse
import Text as T
import Graphics.Element as E
import Graphics.Collage as C


sampleListOn : Signal b -> List a -> Signal a
sampleListOn _ xs =
  -- TODO
  case xs of
    [x] -> Signal.constant x

view : (Int,Int) -> Tree -> E.Element
view _ _ =
  -- TODO
  E.spacer 0 0

signalTree : Signal Tree
signalTree =
  -- TODO
  Signal.constant Empty

main : Signal E.Element
main =
  Signal.map2 view Window.dimensions signalTree
