{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                           (FromJSON, ToJSON)
import           Data.List                            (transpose)
import           GHC.Generics
import           Network.Wai.Middleware.RequestLogger
import           Test.QuickCheck
import           Web.Scotty

type Board = [[Subboard]]

type Subboard = [[Cell]]

data Cell = Empty | Val Int
  deriving (Show, Generic)

instance ToJSON Cell
instance FromJSON Cell

genSubBoard :: Gen Subboard
genSubBoard = do
  a <- shuffle [1 .. 9]
  let b  = [[0,1,2],
            [3,4,5],
            [6,7,8]]
  return $ map (map (\x -> Val (a !! x))) b


genBoard :: Gen Board
genBoard = do
  a <- vectorOf 9 genSubBoard
  let b  = [[0,1,2],
            [3,4,5],
            [6,7,8]]
  let c = map (map (a !!)) b
  if verify c then return c else genBoard

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
             [[[Val 3, Val 1, Val 7],
               [Val 9, Val 2, Val 5],
               [Val 4, Val 6, Val 8]],
              [[Val 2, Val 5, Val 4],
               [Val 1, Val 8, Val 6],
               [Val 3, Val 9, Val 7]],
              [[Val 8, Val 9, Val 6],
               [Val 4, Val 7, Val 3],
               [Val 1, Val 5, Val 2]]],


             [[[Val 2, Val 3, Val 4],
               [Val 5, Val 8, Val 6],
               [Val 1, Val 6, Val 9]],
              [[Val 6, Val 7, Val 1],
               [Val 9, Val 4, Val 2],
               [Val 5, Val 3, Val 8]],
              [[Val 9, Val 8, Val 5],
               [Val 7, Val 3, Val 1],
               [Val 6, Val 2, Val 4]]],

             [[[Val 8, Val 9, Val 1],
               [Val 7, Val 4, Val 2],
               [Val 6, Val 5, Val 3]],
              [[Val 4, Val 2, Val 3],
               [Val 8, Val 6, Val 5],
               [Val 7, Val 1, Val 9]],
              [[Val 5, Val 6, Val 7],
               [Val 3, Val 1, Val 9],
               [Val 2, Val 4, Val 8]]]
          ]
