{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module InliningNoDup where
import Control.Applicative
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div, repeat)
import qualified Prelude as P

import Symantics

newtype InliningNoDup = IND
  { unIND :: (String -> ExpQ) -> (String -> ExpQ -> ExpQ) -> ExpQ
  }

instance SymExp InliningNoDup where
  int n = IND $ \_ _ -> [| (n :: Int) |]
  var s = IND $ \env _ -> env s
  app s (IND e) = IND $ \env fenv -> [|
    let x = $(e env fenv)
    in $(fenv s [| x |])
    |]
  add (IND e1) (IND e2) = IND $ \env fenv ->
    [| $(e1 env fenv) + $(e2 env fenv) |]
  sub (IND e1) (IND e2) = IND $ \env fenv ->
    [| $(e1 env fenv) - $(e2 env fenv) |]
  mul (IND e1) (IND e2) = IND $ \env fenv ->
    [| $(e1 env fenv) * $(e2 env fenv) |]
  div (IND e1) (IND e2) = IND $ \env fenv ->
    [| $(e1 env fenv) `P.div` $(e2 env fenv) |]
  ifz (IND e1) (IND e2) (IND e3) =
    IND $ \env fenv -> [|
      if $(e1 env fenv) == 0
        then $(e2 env fenv)
        else $(e3 env fenv)
    |]

instance SymDecl InliningNoDup where
  declaration s1 s2 (IND e1) (IND e) =
    IND $ \env fenv -> [|
      let f x = $(
            let body cf x = e1 (ext env s2 x) (ext fenv s1 cf)
            in repeat 1 body (\y -> [| f $y |]) [| x |])
      in $(e env (ext fenv s1 (\y -> [| f $y |])))
      |]

instance SymProg InliningNoDup where
  program ds e = foldl' (flip ($)) e ds

evalIND :: InliningNoDup -> ExpQ
evalIND (IND f) = f env0 fenv0

dumpIND :: InliningNoDup -> IO ()
dumpIND staged = runQ (pprint <$> evalIND staged) >>= putStrLn
