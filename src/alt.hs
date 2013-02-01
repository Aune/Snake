module Alt where

import Prelude hiding(Left,Right)
-- State management
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
-- Graphics management
import qualified Graphics.UI.SDL as SDL

-- DATA

data Mode = Play | Menu | Edit | Highscore
            deriving(Read,Show,Eq)

data Position = Pos Int Int
            deriving(Read,Show,Eq)

data Direction = Up | Down | Left | Right
            deriving(Read,Show,Eq)

data Collision = Wall | Food | None
            deriving(Read,Show,Eq)

type Board     = [Position]

data Snake     = Snake [Position] Int
            deriving(Read,Show,Eq)

data GameState = GameState {mode      :: Mode
                           ,direction :: Direction
                           ,board     :: Board
                           ,snake     :: Snake
                           ,surface   :: SDL.Surface}
            deriving(Show,Eq)

type Game a = StateT GameState IO a

-- Graphics

drawHighscore :: Game ()
drawHighscore = undefined

drawMenu :: Game ()
drawMenu = undefined

drawEdit :: Game ()
drawEdit = undefined

drawPlay :: Game ()
drawPlay = undefined

-- CONTROL

changeDirection :: Direction -> Game ()
changeDirection dir = undefined

-- Handlers depending on current mode

handlerMenu :: SDL.Event -> Game ()
handlerMenu = undefined

handlerEdit :: SDL.Event -> Game ()
handlerEdit = undefined

handlerPlay :: SDL.Event -> Game ()
handlerPlay = undefined

handlerHighscore :: SDL.Event -> Game ()
handlerHighscore = undefined

-- Takes an event and dispatshes to apropriate handler

handleEvent :: Game ()
handleEvent = do
  state <- get
  let currentMode = mode state
  event <- io SDL.waitEventBlocking 
  case currentMode of
    Play -> handlerPlay event
    Edit -> handlerEdit event
    Menu -> handlerMenu event
    Highscore -> handlerHighscore event

-- Logic

tick :: Game ()
tick = undefined


-- Utils

io :: IO a -> Game a
io = liftIO

-- Main

main = do
  SDL.init [SDL.InitEverything]
  