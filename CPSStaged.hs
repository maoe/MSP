{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module CPSStaged where
import Control.Applicative
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

newtype CPSStaged = CPSS
  { unCPSS
      :: (String -> ExpQ) -> (String -> ExpQ)
      -> (Maybe ExpQ -> ExpQ) -> ExpQ
  }

instance SymExp CPSStaged where
  int n = CPSS $ \_ _ k -> k $ Just [| (n :: Int) |]
  var s = CPSS $ \env _ k -> k $ Just $ env s
  app s (CPSS e) = CPSS $ \env fenv k ->
    e env fenv $ \case
      Just x -> k $ Just $ [| $(fenv s) $x |]
      Nothing -> k Nothing
  add (CPSS e1) (CPSS e2) = CPSS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x + $y |]
          _ -> k Nothing
  sub (CPSS e1) (CPSS e2) = CPSS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x - $y |]
          _ -> k Nothing
  mul (CPSS e1) (CPSS e2) = CPSS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x * $y |]
          _ -> k Nothing
  div (CPSS e1) (CPSS e2) = CPSS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> [|
            if $y == 0
              then $(k Nothing)
              else $(k $ Just $ [| $x `P.div` $y |])
            |]
          _ -> k Nothing
  ifz (CPSS e1) (CPSS e2) (CPSS e3) =
    CPSS $ \env fenv k ->
      e1 env fenv $ \case
        Just x -> [|
          if $x == 0
            then $(e2 env fenv k)
            else $(e3 env fenv k)
          |]
        Nothing -> k Nothing

instance SymDecl CPSStaged where
  declaration s1 s2 (CPSS e1) (CPSS e) = CPSS $ \env fenv k -> [|
    let f x = $(e1 (ext env s2 [| x |]) (ext fenv s1 [| f |]) k)
    in $(e env (ext fenv s1 [| f |]) k)
    |]

instance SymProg CPSStaged where
  program ds e = foldl' (flip ($)) e ds

evalCPSS :: CPSStaged -> ExpQ
evalCPSS (CPSS f) = f env0 fenv0 (fromMaybe (error "Zero division"))

dumpCPSS :: CPSStaged -> IO ()
dumpCPSS staged = runQ (pprint <$> evalCPSS staged) >>= putStrLn
