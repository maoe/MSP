{-# LANGUAGE TemplateHaskell #-}
module Main where
import Control.DeepSeq (deepseq)
import Criterion.Main

import Symantics (fact15)
import Simple
import Staged
import ErrorHandling
import ErrorHandlingStaged

main :: IO ()
main = defaultMain
  [ bgroup "factorial-15"
      [ bench "simple" $ nf (evalSimple fact15 `deepseq`) ()
      , bench "staged" $ nf ($(evalStaged fact15) `deepseq`) ()
      , bench "error-handling" $ nf (evalEH fact15 `deepseq`) ()
      , bench "error-handling-staged" $ nf ($(evalEHS fact15) `deepseq`) ()
      ]
  ]
