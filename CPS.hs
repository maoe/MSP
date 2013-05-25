{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module CPS where
import Data.Foldable (foldl')
import Data.Maybe
import Prelude hiding (div)
import qualified Prelude as P

import Symantics

newtype CPS r = CPS
  { unCPS
      :: (String -> Int) -> (String -> Int -> Int)
      -> (Maybe Int -> r) -> r
  }

instance SymExp (CPS r) where
  int n = CPS $ \_ _ k -> k $ return n
  var s = CPS $ \env _ k -> k $ return $ env s
  app s (CPS e) = CPS $ \env fenv k ->
    e env fenv $ \case
      Just x -> k $ return $ fenv s x
      Nothing -> k Nothing
  add (CPS e1) (CPS e2) = CPS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ x + y
          _ -> k Nothing
  sub (CPS e1) (CPS e2) = CPS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ x - y
          _ -> k Nothing
  mul (CPS e1) (CPS e2) = CPS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ x * y
          _ -> k Nothing
  div (CPS e1) (CPS e2) = CPS $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) ->
            if x == 0 then k Nothing else k (Just $ x `P.div` y)
          _ -> k Nothing
  ifz (CPS e1) (CPS e2) (CPS e3) = CPS $ \env fenv k ->
    e1 env fenv $ \case
       Just 0 -> e2 env fenv k
       Just x -> e3 env fenv k
       Nothing -> k Nothing

instance SymDecl (CPS Int) where
  declaration s1 s2 (CPS e1) (CPS e) = CPS $ \env fenv k ->
    let f x = e1 (ext env s2 x) (ext fenv s1 f) k
    in e env (ext fenv s1 f) k

instance SymProg (CPS r) where
  program ds e = foldl' (flip ($)) e ds

evalCPS :: CPS Int -> Int
evalCPS (CPS f) = f env0 fenv0 (fromMaybe (error "Zero division"))
