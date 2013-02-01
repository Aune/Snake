module Data where

{-# language -XTypeOperators #-}

import Prelude hiding(length,Left,Right)
import Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as St




data Position  = Pos Int Int
                 deriving(Eq,Read,Show)

data Snake     = Snake {body :: [Position], length :: Int}
                 deriving(Eq,Read,Show)

data Direction = Up | Down | Left | Right
                 deriving(Eq,Read,Show)

data Event     = Collision | Food | NoEvent
                 deriving(Eq,Read,Show)

type Board     = [Position]

type Food      = [Position]

data GameState  = GameState {board :: Board, snake :: Snake, food :: Food, direction :: Direction}
                 deriving(Eq,Read,Show)

type Game a = St.StateT GameState IO a


movePos :: Direction -> Position -> Position
movePos Up    (Pos x y) = Pos (x + 1) y
movePos Down  (Pos x y) = Pos (x - 1) y
movePos Left  (Pos x y) = Pos x (y - 1)
movePos Right (Pos x y) = Pos x (y + 1)

moveSnake :: Direction -> Snake -> Snake
moveSnake dir snake = Snake (take (length snake) (movePos dir (head.body $ snake) : body snake)) (length snake)

checkCollisions :: Position -> GameState -> Event
checkCollisions pos state = case (elem pos (body.snake $ state), elem pos (board state), elem pos (food state)) of
                              (False, False, False) -> NoEvent
                              (False, False, True ) -> Food
                              (_,_,_)               -> Collision

tick :: Maybe Direction -> Game ()
tick (Just dir) = do state <- St.get
                     St.put $ GameState (board state)
                                        (snake state)
                                        (food  state)
                                        dir
                     tick Nothing
tick Nothing    = do state <- St.get
                     let newSnake  = moveSnake (direction state) (snake state)
                         snakeHead = (head (body newSnake))
                         event     = checkCollisions snakeHead state
                     St.put $ GameState (board state) newSnake (food state) (direction state)
                     handleEvent event


handleEvent :: Event -> Game ()
handleEvent event = do state <- St.get
                       case event of 
                         NoEvent -> do run
                         Food    -> do St.put $ GameState (board state)
                                                          (Snake (body.snake $ state) (1 + (length.snake $ state)))
                                                          (filter (/= (head (body.snake $ state))) (food state))
                                                          (direction state)
                                       run
                         Collision ->  io gameOver

getDirection :: IO (Maybe Direction)
getDirection = do char <- getChar
                  let dir = case char of
                              'w' -> Just Up
                              's' -> Just Down
                              'a' -> Just Left
                              'd' -> Just Right
                              _   -> Nothing
                  return dir

run :: Game ()
run = do drawGame
         dir <- io getDirection
         tick dir
       

runGame :: GameState -> IO ()
runGame init = St.evalStateT run init

drawGame :: Game ()
drawGame = do state <- St.get
              io (putStrLn.show $ state)

io :: IO a -> Game a
io = liftIO

gameOver :: IO ()
gameOver = putStrLn "Game Over!"

main = do
  runGame initial
  

initial :: GameState
initial = GameState [(Pos 1 1), (Pos 1 2), (Pos 1 3), (Pos 1 4), (Pos 1 5), (Pos 1 6)]
                    (Snake [(Pos 3 3), (Pos 3 4)] 2)
                    [(Pos 5 5), (Pos 6 6)]
                    Up
                    