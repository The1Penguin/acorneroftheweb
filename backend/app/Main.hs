{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.List
import           Data.Maybe
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Test.QuickCheck
import           Web.Scotty

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
genBoard board e@(v:vs) =
        if verify board then return board else do
          let (y1, x1, y2, x2) = case findEmpty board of
                Just (a,b,c,d) -> (a,b,c,d)
                Nothing        -> (3,3,3,3)
          let new =
                [[[[
                      if (board !! y !! x !! b !! a) /= Empty
                      then board !! y !! x !! b !! a
                      else
                        (if y1==y && x1 == x && y2 == b && x2 == a
                         then Val v
                         else Empty)
                   | a <- [0..2]]
                  | b <- [0..2]]
                 | x <- [0..2]]
                | y <- [0..2]]
          if valid new then genBoard new vs else genBoard board (vs ++ [v])


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
    cols = all listValid $ fetchRows x

verify :: Board -> Bool
verify x = squares && rows && cols
  where
    squares = and $ concatMap (map squareVerify) x
    rows = all listVerify $ fetchRows x
    cols = all listVerify $ fetchRows x

main :: IO ()
main = scotty 3000 $ do
  middleware logStdout

  get "/generate" $ do
    json (undefined :: Board)

  get "/example" $ do
    json example

  get "/generate" $ do
    json $ Empty

  post "/solve" $ do
      board <- jsonData
      json $ verify board


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
           [Empty | _ <- [0..2]]
          | _ <- [0..2]]
         | _ <- [0..2]]
        | _ <- [0..2]]

temp :: Board
temp = [
             [[[Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7]],
              [[Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7]],
              [[Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7]]],
             [[[Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7],
               [Val 9, Val 6, Val 7]],
              [[Val 9, Val 6, Val 7],
               [Val 9, Empty, Empty],
               [Empty, Empty, Empty]],
              [[Empty, Empty, Empty],
               [Empty, Empty, Empty],
               [Empty, Empty, Empty]]],
             [[[Empty, Empty, Empty],
               [Empty, Empty, Empty],
               [Empty, Empty, Empty]],
              [[Empty, Empty, Empty],
               [Empty, Empty, Empty],
               [Empty, Empty, Empty]],
              [[Empty, Empty, Empty],
               [Empty, Empty, Empty],
               [Empty, Empty, Empty]]]
          ]
