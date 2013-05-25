module ErrorHandling where
import Control.Applicative
import Data.Foldable (foldl')
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

-- Error Handling

newtype ErrorHandling = EH
  { unEH :: (String -> Int) -> (String -> Int -> Maybe Int) -> Maybe Int
  }

instance SymExp ErrorHandling where
  int n = EH $ \_ _ -> return n
  var s = EH $ \env _ -> return $ env s
  app s (EH e) = EH $ \env fenv -> do
    x <- e env fenv
    fenv s x
  add (EH e1) (EH e2) = EH $ \env fenv ->
    liftA2 (+) (e1 env fenv) (e2 env fenv)
  sub (EH e1) (EH e2) = EH $ \env fenv ->
    liftA2 (-) (e1 env fenv) (e2 env fenv)
  mul (EH e1) (EH e2) = EH $ \env fenv ->
    liftA2 (*) (e1 env fenv) (e2 env fenv)
  div (EH e1) (EH e2) = EH $ \env fenv -> do
    x2 <- e2 env fenv
    if x2 == 0
      then fail "Zero division"
      else do
        x1 <- e1 env fenv
        return $ x1 `P.div` x2
  ifz (EH e1) (EH e2) (EH e3) =
    EH $ \env fenv -> do
      x <- e1 env fenv
      if x == 0 then e2 env fenv else e3 env fenv

instance SymDecl ErrorHandling where
  declaration s1 s2 (EH e1) (EH e) = EH $ \env fenv ->
    let f x = e1 (ext env s2 x) (ext fenv s1 f)
    in e env (ext fenv s1 f)

instance SymProg ErrorHandling where
  program ds e = foldl' (flip ($)) e ds

evalEH :: ErrorHandling -> Maybe Int
evalEH (EH f) = f env0 fenv0

