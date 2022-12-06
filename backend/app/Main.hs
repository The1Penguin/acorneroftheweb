{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad                        (replicateM)
import           Control.Monad.IO.Class
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.IORef
import           Data.List                            (nub, transpose)
import           Data.Maybe
import           GHC.Generics
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.RequestLogger
import           Prelude                              hiding (lookup)
import           System.Random
import           System.Random.Shuffle
import           Test.QuickCheck
import           Web.Scotty

type Board = [[Cell]]

data Cell = Empty | Val Int
  deriving (Show, Generic, Eq)

instance ToJSON Cell
instance FromJSON Cell

findEmpty :: Board -> [(Int, Int)]
findEmpty board = concat $
                [[
                     (y,x)
                   | x <- [0..8], board !! y !! x  == Empty]
                | y <- [0..8]]

optionBoard :: Board -> (Int, Int) -> [Cell]
optionBoard board (row, col) = [i | i <- map Val [1..9], i `notElem` left]
  where
    rows = fetchRows board
    cols = fetchCols board
    leftOnRow = filter (/= Empty) (rows !! row)
    leftOnCol = filter (/= Empty) (cols !! col)
    left = leftOnRow ++ leftOnCol

genBoard :: IO Board
genBoard = fromJust . solve empty <$> newStdGen

updateBoard :: Board -> (Int, Int) -> Cell -> Board
updateBoard board (y1,x1) val =
                [[
                      if y1 == y && x1 == x
                      then val
                      else board !! y !! x
                 | x <- [0..8]]
                | y <- [0..8]]

solve :: Board -> StdGen -> Maybe Board
solve board std
  | not (valid board) = Nothing
  | verify board = Just board
  | otherwise = if null solutions then Nothing else head solutions
    where
      poss = findEmpty board
      pos = head $ shuffle' poss (length poss) std
      solutions = filter (/= Nothing)
        [solve (updateBoard board pos val) std | val <- optionBoard board pos]


removeBoard :: Board -> Int -> Gen Board
removeBoard board 0 = return board
removeBoard board amount = do
          x1 <- elements [0..8]
          y1 <- elements [0..8]
          let new =
                [[
                      if y1==y && x1 == x
                      then Empty
                      else board !! y !! x
                 | x <- [0..8]]
                | y <- [0..8]]
          removeBoard new (amount-1)


fetchRows :: Board -> [[Cell]]
fetchRows = id

fetchCols :: Board -> [[Cell]]
fetchCols = transpose

fetchSquares :: Board -> [[Cell]]
fetchSquares = map concat . groupBy3 . concat . transpose . map groupBy3
  where
    groupBy3 :: [t] -> [[t]]
    groupBy3 []         = []
    groupBy3 [_]        = error "The sudoku board became malformed"
    groupBy3 [_,_]      = error "The sudoku board became malformed"
    groupBy3 (a:b:c:ds) = [a,b,c] : groupBy3 ds

listVerify :: [Cell] -> Bool
listVerify = (== 45) . foldr (\x -> (+) $ case x of
                                 Empty -> 0
                                 Val i -> i
                             ) 0

listValid :: [Cell] -> Bool
listValid x = a == b
  where
    a = filter (/= Empty) x
    b = nub a

valid :: Board -> Bool
valid x = squares && rows && cols
  where
    squares = all listValid $ fetchSquares x
    rows    = all listValid $ fetchRows x
    cols    = all listValid $ fetchCols x

verify :: Board -> Bool
verify x = rows && cols
  where
    -- squares = all listVerify $ fetchSquares x
    rows    = all listVerify $ fetchRows x
    cols    = all listVerify $ fetchCols x

routes :: ScottyM ()
routes = do
  middleware logStdout
  middleware simpleCors

  get "/example" $ do
    json example

  get "/generate" $ do
      a <- liftIO $ (generate . flip removeBoard 40) =<< genBoard
      json a

  post "/solve" $ do
      board <- jsonData
      json $ verify board

update :: IORef [Board] -> [Board] -> IO ()
update board list= do
  a <- genBoard
  writeIORef board (list ++ [a])
  putStrLn "Added a board to the list"

main :: IO ()
main = scotty 3000 routes

example :: Board
example = [[Val 5, Val 3, Val 4, Val 6, Val 7, Val 8, Val 9, Val 1, Val 2],
           [Val 6, Val 7, Val 2, Val 1, Val 9, Val 5, Val 3, Val 4, Val 8],
           [Val 1, Val 9, Val 8, Val 3, Val 4, Val 2, Val 5, Val 6, Val 7],
           [Val 8, Val 5, Val 9, Val 7, Val 6, Val 1, Val 4, Val 2, Val 3],
           [Val 4, Val 2, Val 6, Val 8, Val 5, Val 3, Val 7, Val 9, Val 1],
           [Val 7, Val 1, Val 3, Val 9, Val 2, Val 4, Val 8, Val 5, Val 6],
           [Val 9, Val 6, Val 1, Val 5, Val 3, Val 7, Val 2, Val 8, Val 4],
           [Val 2, Val 8, Val 7, Val 4, Val 1, Val 9, Val 6, Val 3, Val 5],
           [Val 3, Val 4, Val 5, Val 2, Val 8, Val 6, Val 1, Val 7, Val 9]]


empty :: Board
empty = [
          [ Empty
         | _ <- [1..9 :: Integer]]
        | _ <- [1..9 :: Integer]]
