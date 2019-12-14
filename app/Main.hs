{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Lens
import Network.Wreq
import Data.Aeson (decode)
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import qualified Weather as W
import Data.Maybe

newtype Location a = Location a deriving (Show)

getWeather :: Location String -> IO (Response B.ByteString)
getWeather (Location loc) = get ("http://api.openweathermap.org/data/2.5/weather?q=" ++ loc ++ "&APPID=45c5f1fd30e1f5856fcc69a28c3136c9&units=imperial")

parseWeather :: Response B.ByteString -> Maybe W.CurrentWeather
parseWeather weather = decode $ weather ^. responseBody

printWeather :: W.CurrentWeather -> String
printWeather (W.CurrentWeather { .. }) = "It is " ++ (show $ W.temp main) ++ " in " ++ name ++ " but it feels like, " ++ (show $ W.feels_like main) 
printWeather (W.NoLocation v) = v

handleWeather :: Maybe W.CurrentWeather -> W.CurrentWeather
handleWeather (Just a) = a
handleWeather Nothing = W.NoLocation "No valid location"

main :: IO ()
main = do
  Prelude.putStrLn "Enter your city"
  loc <- Location <$> getLine
  weather <- getWeather loc
  print $ (printWeather . handleWeather . parseWeather) weather

