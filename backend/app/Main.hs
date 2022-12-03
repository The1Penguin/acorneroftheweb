{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class
import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.IORef
import           Data.List ( nub, transpose, elemIndex )
import           Data.Maybe
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Cors
import           Test.QuickCheck
import           Web.Scotty
import           Prelude hiding (lookup)

type Board = [[Subboard]]

type Subboard = [[Cell]]

data Cell = Empty | Val Int
  deriving (Show, Generic, Eq)

instance ToJSON Cell
instance FromJSON Cell

findEmpty :: Board -> Maybe (Int, Int, Int, Int)
findEmpty board = if isJust index then Just (y1, x1, y2, x2) else Nothing
  where
    list  = concat $ concat $ concat board
    index = elemIndex Empty list
    indexFix = fromJust index
    y1 = indexFix `div` 27
    x1 = (`div` 9) $ indexFix `mod` 27
    y2 = (`div` 3) $ indexFix `mod` 9
    x2 = indexFix `mod` 3

genBoard :: Board -> [Int] -> Gen Board
genBoard board [] = do
  let a = [1..9]
  b <- shuffle a
  genBoard board b
genBoard board vs =
        if verify board then return board else do
          let (y1, _, y2, _) = case findEmpty board of
                Just (a,b,c,d) -> (a,b,c,d)
                Nothing        -> (3,3,3,3)
          let new =
                [[[[
                      if (board !! y !! x !! b !! a) /= Empty
                      then board !! y !! x !! b !! a
                      else
                        (if y1==y && y2 == b
                         then Val (vs !! (3*x+a))
                         else Empty)
                   | a <- [0..2]]
                  | b <- [0..2]]
                 | x <- [0..2]]
                | y <- [0..2]]
          if valid new then genBoard new [] else genBoard board []

removeBoard :: Board -> Int -> Gen Board
removeBoard board 0 = return board
removeBoard board amount = do
          x1 <- elements [0..2]
          x2 <- elements [0..2]
          y1 <- elements [0..2]
          y2 <- elements [0..2]
          let new =
                [[[[
                      if y1==y && x1 == x && y2 == b && x2 == a
                      then Empty
                      else board !! y !! x !! b !! a
                   | a <- [0..2]]
                  | b <- [0..2]]
                 | x <- [0..2]]
                | y <- [0..2]]
          removeBoard new (amount-1)


fetchRows :: Board -> [[Cell]]
fetchRows x = foldr (\y -> (++) $ map (concatMap (!! y)) x) [] [0..2]

fetchCols :: Board -> [[Cell]]
fetchCols = fetchRows . transpose . map (map transpose)

squareVerify :: Subboard -> Bool
squareVerify = listVerify . concat

listVerify :: [Cell] -> Bool
listVerify = (== 45) . foldr (\x -> (+) $ case x of
                                 Empty -> 0
                                 Val i -> i
                             ) 0

squareValid :: Subboard -> Bool
squareValid = listValid . concat

listValid :: [Cell] -> Bool
listValid x = a == b
  where
    a = filter (/= Empty) x
    b = nub a

valid :: Board -> Bool
valid x = squares && rows && cols
  where
    squares = and $ concatMap (map squareValid) x
    rows = all listValid $ fetchRows x
    cols = all listValid $ fetchCols x

verify :: Board -> Bool
verify x = squares && rows && cols
  where
    squares = and $ concatMap (map squareVerify) x
    rows = all listVerify $ fetchRows x
    cols = all listVerify $ fetchCols x

routes :: IORef [Board] -> ScottyM ()
routes boards = do
  middleware logStdout
  middleware simpleCors

  get "/example" $ do
    json example

  get "/generate" $ do
      a <- liftIO $ readIORef boards
      let b = head a
      _ <- liftIO $ forkIO $ update boards (tail a)
      json b

  post "/solve" $ do
      board <- jsonData
      json $ verify board

update :: IORef [Board] -> [Board] -> IO ()
update board list= do
  a <- generate $ genBoard empty []
  writeIORef board (list ++ [a])
  putStrLn "Added a board to the list"

main :: IO ()
main = do
  a <- replicateM 3 $ (generate . flip removeBoard 40)  =<< generate (genBoard empty [])
  print a
  c <- newIORef a
  scotty 3000 $ routes c

example :: Board
example = [
             [[[Val 9, Val 6, Val 7],
               [Val 4, Val 8, Val 1],
               [Val 5, Val 2, Val 3]],
              [[Val 1, Val 3, Val 2],
               [Val 6, Val 5, Val 7],
               [Val 4, Val 9, Val 8]],
              [[Val 4, Val 5, Val 8],
               [Val 2, Val 3, Val 9],
               [Val 1, Val 6, Val 7]]],

             [[[Val 2, Val 3, Val 4],
               [Val 1, Val 5, Val 6],
               [Val 7, Val 9, Val 8]],
              [[Val 8, Val 1, Val 9],
               [Val 7, Val 2, Val 3],
               [Val 5, Val 4, Val 6]],
              [[Val 5, Val 7, Val 6],
               [Val 9, Val 8, Val 4],
               [Val 3, Val 2, Val 1]]],

             [[[Val 6, Val 4, Val 2],
               [Val 3, Val 7, Val 9],
               [Val 8, Val 1, Val 5]],
              [[Val 9, Val 8, Val 5],
               [Val 2, Val 6, Val 1],
               [Val 3, Val 7, Val 4]],
              [[Val 7, Val 1, Val 3],
               [Val 8, Val 4, Val 5],
               [Val 6, Val 9, Val 2]]]
          ]


empty :: Board
empty = [
         [
          [
           [Empty | _ <- [0..2 :: Integer]]
          | _ <- [0..2 :: Integer]]
         | _ <- [0..2 :: Integer]]
        | _ <- [0..2 :: Integer]]
