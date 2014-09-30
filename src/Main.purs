module Main where

import Data.Function

import Control.Monad.Eff
import Control.Apply

import Graphics.Canvas

import Signal
import Signal.Time
import Signal.DOM

import DOM

data Direction = Left | Right

type Mario =
  { x :: Number, y :: Number
  , vx :: Number, vy :: Number
  , dir :: Direction
  }

type Inputs = { right :: Boolean
              , left :: Boolean
              , jump :: Boolean }

-- SETTINGS --

jumpKeyCode = 38 -- up arrow
rightKeyCode = 39 -- right arrow
leftKeyCode = 37 -- left arrow

gravityAccelerate = 0.98
jumpAccelerate = 10
moveSpeed = 3
frameRate = fps 25

groundLevel = 200

-- /SETTINGS --

initMario :: Mario
initMario = { x: 0, y: 0
            , vx: 0, vy: 0
            , dir: Right }

fps :: Number -> Time
fps = (/) second

solidGround :: Mario -> Mario
solidGround o | not (airborn o) = o { y = 0, vy = 0 }
solidGround o = o

gravity :: Mario -> Mario
gravity o = o { vy = o.vy + gravityAccelerate }

velocity :: Mario -> Mario
velocity o = o { x = o.x + o.vx
               , y = o.y + o.vy }


airborn :: Mario -> Boolean
airborn m = m.y < 0

jump :: Boolean -> Mario -> Mario
jump true m | not (airborn m) = m { vy = -jumpAccelerate }
jump true m = m
jump false m = m

walk :: Boolean -> Boolean -> Mario -> Mario
walk true false m = m { vx = -moveSpeed, dir = Left }
walk false true m = m { vx = moveSpeed, dir = Right }
walk false false m | airborn m = m
walk true true m = m { vx = 0 }
walk false false m = m { vx = 0 }

marioLogic :: Inputs -> Mario -> Mario
marioLogic inputs = solidGround <<< velocity <<< gravity
                  <<< walk inputs.left inputs.right
                  <<< jump inputs.jump

mkInputs :: Boolean -> Boolean -> Boolean -> Inputs
mkInputs l r j = { right: r, left: l, jump: j }

mario :: Signal Inputs -> Signal Time -> Signal Mario
mario keys tick = foldp marioLogic initMario (sampleOn tick keys)

main = do
  jumpKey <- keyPressed jumpKeyCode
  rightKey <- keyPressed rightKeyCode
  leftKey <- keyPressed leftKeyCode
  window <- windowDimensions
  let inputs = mkInputs <$> leftKey <*> rightKey <*> jumpKey
      frame = every frameRate
      ground = window.height - groundLevel
  background window ground
  runSignal $ mario inputs frame ~> render ground

render :: forall eff. Number -> Mario -> Eff (dom :: DOM | eff) Unit
render ground m = let dir = case m.dir of
                                      Right -> "right"
                                      Left -> "left"
                      verb = if airborn m
                                then "jump"
                                else if m.vx == 0
                                then "stand" else "walk"
                  in runFn4 renderImpl verb dir m.x (m.y + ground)

background :: forall eff. Dimensions -> Number -> Eff (dom :: DOM, canvas :: Canvas | eff) Unit
background window ground = do
  el <- getCanvasElementById "background"
  ctx <- getContext2D el
  setCanvasDimensions {width: window.width, height: window.height} el
  setFillStyle "#AEEEEE" ctx
  fillRect ctx {h: window.height, w: window.width, x: 0, y: 0}
  setFillStyle "#4AA329" ctx
  fillRect ctx {h: window.height - ground, w: window.width, x: 0, y: ground}
  return unit

foreign import renderImpl
  "function renderImpl(verb, dir, x, y) {\
  \  return function () {\
  \    var el = document.getElementById('mario');\
  \    var src = 'images/' + verb + '-' + dir + '.gif';\
  \    el.setAttribute('style',\
  \                   'background-image: url(' + src + ');'\
  \                   + 'width: 35px;'\
  \                   + 'height: 35px;'\
  \                   + 'transform: matrix(1, 0, 0, 1, ' + x + ', ' + (y - 30)  + ')');\
  \    return {};\
  \  };\
  \}" :: forall eff. Fn4 String String Number Number (Eff (dom :: DOM | eff) Unit)


windowDimensions :: forall eff. Eff (dom :: DOM | eff) Dimensions
windowDimensions = do
  width <- windowWidth
  height <- windowHeight
  return { width: width, height: height }

foreign import windowWidth
  "function windowWidth() {\
  \    return window.document.documentElement.clientWidth;\
  \}" :: forall eff. Eff (dom:: DOM | eff) Number

foreign import windowHeight
  "function windowHeight() {\
  \    return window.document.documentElement.clientHeight;\
  \}" :: forall eff. Eff (dom:: DOM | eff) Number

