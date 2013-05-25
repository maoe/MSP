{-# LANGUAGE TemplateHaskell #-}
module Staged where
import Control.Applicative
import Data.Foldable (foldl')
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

-- The Simple Interpreter Staged

newtype Staged = Staged
  { unStaged :: (String -> ExpQ) -> (String -> ExpQ) -> ExpQ
  }

instance SymExp Staged where
  int n = Staged $ \_ _ -> [| (n :: Int) |]
  var s = Staged $ \env _ -> env s
  app s (Staged e) = Staged $ \env fenv ->
    [| $(fenv s) $(e env fenv) |]
  add (Staged e1) (Staged e2) = Staged $ \env fenv ->
    [| $(e1 env fenv) + $(e2 env fenv) |]
  sub (Staged e1) (Staged e2) = Staged $ \env fenv ->
    [| $(e1 env fenv) - $(e2 env fenv) |]
  mul (Staged e1) (Staged e2) = Staged $ \env fenv ->
    [| $(e1 env fenv) * $(e2 env fenv) |]
  div (Staged e1) (Staged e2) = Staged $ \env fenv ->
    [| $(e1 env fenv) `P.div` $(e2 env fenv) |]
  ifz (Staged e1) (Staged e2) (Staged e3) =
    Staged $ \env fenv -> [|
      if $(e1 env fenv) == 0
        then $(e2 env fenv)
        else $(e3 env fenv)
    |]

instance SymDecl Staged where
  declaration s1 s2 (Staged e1) (Staged e) = Staged $ \env fenv -> [|
      let f x = $(e1 (ext env s2 [| x |]) (ext fenv s1 [| f |]))
      in $(e env (ext fenv s1 [| f |]))
    |]

instance SymProg Staged where
  program ds e = foldl' (flip ($)) e ds

evalStaged :: Staged -> ExpQ
evalStaged (Staged f) = f env0 fenv0

dumpStaged :: Staged -> IO ()
dumpStaged staged = runQ (pprint <$> evalStaged staged) >>= putStrLn
