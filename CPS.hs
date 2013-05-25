module CPS where

newtype CPS = CPS
  { unCPS :: (String -> Int) -> (String -> Int -> Int) -> (Maybe Int -> a) -> a
  }
