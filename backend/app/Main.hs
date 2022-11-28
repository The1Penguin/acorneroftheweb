{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.List
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

-- Idea, input board, check if done, else add to empty, verify it is valid then recursion
genBoard :: Board -> Gen Board
genBoard board =
  if verify board then return board else do
    undefined

fetchRows :: Board -> [[Cell]]
fetchRows x = foldr (\y -> (++) $ map (concatMap (!! y)) x) [] [0,1,2]

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
             [[[Empty, Empty, Empty],
               [Empty, Empty, Empty],
               [Empty, Empty, Empty]],
              [[Empty, Empty, Empty],
               [Empty, Empty, Empty],
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
