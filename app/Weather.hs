{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Weather where

import Data.Aeson
import GHC.Generics
import Data.Foldable

data Main = Main {
                     temp :: Double
                   -- , feels_like :: Double
                   -- , temp_max :: Double
                 } deriving (Show, Eq, Generic)

data CurrentWeather = CurrentWeather { 
                          main :: Main
                        , name :: String
                      } 
                    | NoLocation String  deriving (Show, Eq, Generic)

instance FromJSON CurrentWeather where
  parseJSON = withObject "CurrentWeather" $ \o -> asum [
                                                      do      
                                                        name             <- o .: "name"
                                                        (main :: Main)    <- o .: "main"
                                                        return CurrentWeather{ .. }
                                                       , do 
                                                           pure $ NoLocation "No Valid Location" 
                                                       ]

instance ToJSON CurrentWeather

instance FromJSON Main
instance ToJSON Main
