{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Inlining where
import Control.Applicative
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div, repeat)
import qualified Prelude as P

import Symantics

newtype Inlining = Inlining
  { unInlining :: (String -> ExpQ) -> (String -> ExpQ -> ExpQ) -> ExpQ
  }

instance SymExp Inlining where
  int n = Inlining $ \_ _ -> [| (n :: Int) |]
  var s = Inlining $ \env _ -> env s
  app s (Inlining e) = Inlining $ \env fenv ->
    fenv s (e env fenv)
  add (Inlining e1) (Inlining e2) = Inlining $ \env fenv ->
    [| $(e1 env fenv) + $(e2 env fenv) |]
  sub (Inlining e1) (Inlining e2) = Inlining $ \env fenv ->
    [| $(e1 env fenv) - $(e2 env fenv) |]
  mul (Inlining e1) (Inlining e2) = Inlining $ \env fenv ->
    [| $(e1 env fenv) * $(e2 env fenv) |]
  div (Inlining e1) (Inlining e2) = Inlining $ \env fenv ->
    [| $(e1 env fenv) `P.div` $(e2 env fenv) |]
  ifz (Inlining e1) (Inlining e2) (Inlining e3) =
    Inlining $ \env fenv -> [|
      if $(e1 env fenv) == 0
        then $(e2 env fenv)
        else $(e3 env fenv)
    |]

repeat :: Int -> (a -> a) -> a -> a
repeat 0 f = f
repeat n f = f . repeat (n-1) f

instance SymDecl Inlining where
  declaration s1 s2 (Inlining e1) (Inlining e) =
    Inlining $ \env fenv -> [|
      let f x = $(
            let body cf x = e1 (ext env s2 x) (ext fenv s1 cf)
            in repeat 1 body (\y -> [| f $y |]) [| x |])
      in $(e env (ext fenv s1 (\y -> [| f $y |])))
      |]

instance SymProg Inlining where
  program ds e = foldl' (flip ($)) e ds

evalInlining :: Inlining -> ExpQ
evalInlining (Inlining f) = f env0 fenv0

dumpInlining :: Inlining -> IO ()
dumpInlining staged = runQ (pprint <$> evalInlining staged) >>= putStrLn
