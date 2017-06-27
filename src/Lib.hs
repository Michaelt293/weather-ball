{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.Time.Clock (UTCTime)
import Data.Attoparsec.Time (utcTime)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (sum, foldl')
import Data.Attoparsec.Text
import Control.Applicative

data Observatory = AU
                 | US
                 | FR
                 | Other
                 deriving (Show, Eq, Ord)

newtype Celsius = Celsius { getCelsius :: Double }
  deriving (Show, Eq, Ord, Num, Fractional)

newtype Km = Km { getKm :: Double }
  deriving (Show, Eq, Ord, Num, Fractional)

data Location = Location {
    x :: Km
  , y :: Km
  } deriving (Show, Eq, Ord)

data Observation = Observation {
    timestamp   :: UTCTime
  , location    :: Location
  , temperature :: Celsius
  , observatory :: Observatory
  } deriving (Show, Eq, Ord)

observatoryParser :: Parser Observatory
observatoryParser =
      (string "AU"  >> return AU)
  <|> (string "US"  >> return US)
  <|> (string "FA"  >> return FR)
  <|> (many1 letter >> return Other)

observationParser :: Parser Observation
observationParser =
  let fahrenheitToCelsius f = Celsius $ (f - 32) / 1.8
      kelvinToCelsius k     = Celsius $ k - 273.15
      milesToKm m           = Km $ m / 0.62137
      mToKm m               = Km $ m / 1000
  in do
    timestamp <- utcTime
    char '|'
    x <- decimal
    char ','
    y <- decimal
    char '|'
    temperature <- signed decimal
    char '|'
    observatory <- observatoryParser
    let mkObservation f g = Observation
                              timestamp
                              (Location (f (fromIntegral x)) (f (fromIntegral y)))
                              (g (fromIntegral temperature))
                              observatory
    return $ case observatory of
      AU    -> mkObservation id id
      US    -> mkObservation milesToKm fahrenheitToCelsius
      FR    -> mkObservation mToKm kelvinToCelsius
      Other -> mkObservation id kelvinToCelsius

observationsParser :: Parser [Observation]
observationsParser = many1 (observationParser <* char '\n')

data Statistics = Statistics {
    minTemp  :: Celsius
  , maxTemp  :: Celsius
  , sumTemp  :: Celsius
  , numOfobs :: Map Observatory Int
  } deriving (Show, Eq, Ord)

statistics :: [Observation] -> Statistics
statistics = foldl' f (Statistics
                         100000
                         (-100000)
                         0
                         (Map.fromList [(AU, 0), (US, 0), (FR, 0), (Other, 0)]))
  where f acc x =
          let min' = minTemp acc
              max' = maxTemp acc
              temp = temperature x
           in Statistics {
                  minTemp  = if min' < temp then min' else temp
                , maxTemp  = if max' > temp then max' else temp
                , sumTemp  = temp + sumTemp acc
                , numOfobs = Map.adjust (+1) (observatory x) (numOfobs acc)
                }

meanTemp :: Statistics -> Celsius
meanTemp s = sumTemp s / fromIntegral (sum (Map.elems (numOfobs s)))
