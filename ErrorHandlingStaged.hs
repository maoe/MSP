{-# LANGUAGE TemplateHaskell #-}
module ErrorHandlingStaged where
import Control.Applicative
import Data.Foldable (foldl')
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

-- Staged with Error Handling

newtype ErrorHandlingStaged = EHS
  { unEHS :: (String -> ExpQ) -> (String -> ExpQ) -> ExpQ
  }

instance SymExp ErrorHandlingStaged where
  int n = EHS $ \_ _ -> [| Just (n :: Int) |]
  var s = EHS $ \env _ -> [| return $(env s) |]
  app s (EHS e) = EHS $ \env fenv -> [| do
    x <- $(e env fenv)
    $(fenv s) x
    |]
  add (EHS e1) (EHS e2) = EHS $ \env fenv ->
    [| liftA2 (+) $(e1 env fenv) $(e2 env fenv) |]
  sub (EHS e1) (EHS e2) = EHS $ \env fenv ->
    [| liftA2 (-) $(e1 env fenv) $(e2 env fenv) |]
  mul (EHS e1) (EHS e2) = EHS $ \env fenv ->
    [| liftA2 (*) $(e1 env fenv) $(e2 env fenv) |]
  div (EHS e1) (EHS e2) = EHS $ \env fenv -> [| do
    x2 <- $(e2 env fenv)
    if x2 == 0
      then fail "Zero division"
      else do
        x1 <- $(e1 env fenv)
        return $ x1 `P.div` x2
    |]
  ifz (EHS e1) (EHS e2) (EHS e3) =
    EHS $ \env fenv -> [| do
      x <- $(e1 env fenv)
      if x == 0 then $(e2 env fenv) else $(e3 env fenv)
      |]

instance SymDecl ErrorHandlingStaged where
  declaration s1 s2 (EHS e1) (EHS e) = EHS $ \env fenv -> [|
      let f x = $(e1 (ext env s2 [| x |]) (ext fenv s1 [| f |]))
      in $(e env (ext fenv s1 [| f |]))
    |]

instance SymProg ErrorHandlingStaged where
  program ds e = foldl' (flip ($)) e ds

evalEHS :: ErrorHandlingStaged -> ExpQ
evalEHS (EHS f) = f env0 fenv0

dumpEHS :: ErrorHandlingStaged -> IO ()
dumpEHS staged = runQ (pprint <$> evalEHS staged) >>= putStrLn
