module Simple where
import Data.Foldable (foldl')
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

-- A Simple Interpreter

newtype Simple = Simple
  { unSimple :: (String -> Int) -> (String -> Int -> Int) -> Int
  }

instance SymExp Simple where
  int n = Simple $ \_ _ -> n
  var s = Simple $ \env _ -> env s
  app s (Simple e) = Simple $ \env fenv ->
    fenv s (e env fenv)
  add (Simple e1) (Simple e2) = Simple $ \env fenv ->
    e1 env fenv + e2 env fenv
  sub (Simple e1) (Simple e2) = Simple $ \env fenv ->
    e1 env fenv - e2 env fenv
  mul (Simple e1) (Simple e2) = Simple $ \env fenv ->
    e1 env fenv * e2 env fenv
  div (Simple e1) (Simple e2) = Simple $ \env fenv ->
    e1 env fenv `P.div` e2 env fenv
  ifz (Simple e1) (Simple e2) (Simple e3) =
    Simple $ \env fenv ->
      if e1 env fenv == 0 then e2 env fenv else e3 env fenv

instance SymDecl Simple where
  declaration s1 s2 (Simple e1) (Simple e) = Simple $ \env fenv ->
    let f x = e1 (ext env s2 x) (ext fenv s1 f)
    in e env (ext fenv s1 f)

instance SymProg Simple where
  program ds e = foldl' (flip ($)) e ds

evalSimple :: Simple -> Int
evalSimple (Simple f) = f env0 fenv0
