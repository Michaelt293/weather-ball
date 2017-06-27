{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text.IO as T
import Data.Monoid ((<>))
import Data.Foldable (traverse_)
import Data.Map (toList)

main :: IO ()
main = do
  text <- T.readFile "observations.txt"
  let obs = case parseOnly observationsParser text of
              Left err -> fail ("Parse error: " <> err)
              Right v -> v
  let stats = statistics obs
  putStrLn $ "The minimum temperature: " <> show (getCelsius (minTemp stats))
  putStrLn $ "The maximum temperature: " <> show (getCelsius (maxTemp stats))
  putStrLn $ "The mean temperature: " <> show (getCelsius (meanTemp stats))
  putStrLn "The number of observations from each observatory: "
  traverse_ print (toList (numOfobs stats))
