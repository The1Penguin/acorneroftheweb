{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List                            (transpose)
import           Network.Wai.Middleware.RequestLogger
import           Web.Scotty

type Board = [[Subboard]]

type Subboard = [[Int]]

fetchRows :: Board -> [[Int]]
fetchRows x = foldr (\y -> (++) $ map (concatMap (!! y)) x) [] [0,1,2]

fetchCols :: Board -> [[Int]]
fetchCols = fetchRows . transpose . map (map transpose)

squareVerify :: Subboard -> Bool
squareVerify = listVerify . concat

listVerify :: [Int] -> Bool
listVerify = (== 45) . sum

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
    json $ (undefined :: String)

  post "/solve" $ do
    json $ True

example :: Board
example = [
             [[[3,1,7],
               [9,2,5],
               [4,6,8]],
              [[2,5,4],
               [1,8,6],
               [3,9,7]],
              [[8,9,6],
               [4,7,3],
               [1,5,2]]],


             [[[2,3,4],
               [5,8,6],
               [1,6,9]],
              [[6,7,1],
               [9,4,2],
               [5,3,8]],
              [[9,8,5],
               [7,3,1],
               [6,2,4]]],

             [[[8,9,1],
               [7,4,2],
               [6,5,3]],
              [[4,2,3],
               [8,6,5],
               [7,1,9]],
              [[5,6,7],
               [3,1,9],
               [2,4,8]]]
          ]
