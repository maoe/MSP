{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module All where
import Control.Applicative
import Data.Foldable (foldl')
import Data.Maybe (fromMaybe)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude hiding (div, repeat)
import qualified Prelude as P

import Symantics

newtype All = All
  { unAll
      :: (String -> ExpQ) -> (String -> ExpQ -> ExpQ)
      -> (Maybe ExpQ -> ExpQ) -> ExpQ
  }

instance SymExp All where
  int n = All $ \_ _ k -> k $ Just [| (n :: Int) |]
  var s = All $ \env _ k -> k $ Just $ env s
  app s (All e) = All $ \env fenv k ->
    e env fenv $ \case
      Just x -> k $ Just $ fenv s x
      Nothing -> k Nothing
  add (All e1) (All e2) = All $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x + $y |]
          _ -> k Nothing
  sub (All e1) (All e2) = All $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x - $y |]
          _ -> k Nothing
  mul (All e1) (All e2) = All $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> k $ Just $ [| $x * $y |]
          _ -> k Nothing
  div (All e1) (All e2) = All $ \env fenv k ->
    e1 env fenv $ \r ->
      e2 env fenv $ \s ->
        case (r, s) of
          (Just x, Just y) -> [|
            if $y == 0
              then $(k Nothing)
              else $(k $ Just $ [| $x `P.div` $y |])
            |]
          _ -> k Nothing
  ifz (All e1) (All e2) (All e3) =
    All $ \env fenv k ->
      e1 env fenv $ \case
        Just x -> [|
          if $x == 0
            then $(e2 env fenv k)
            else $(e3 env fenv k)
          |]
        Nothing -> k Nothing

instance SymDecl All where
  declaration s1 s2 (All e1) (All e) = All $ \env fenv k -> [|
    let f x = $(
          let body cf x = e1 (ext env s2 x) (ext fenv s1 cf) k
          in repeat 1 body (\y -> [| f $y |]) [| x |])
    in $(e env (ext fenv s1 (\y -> [| f $y |])) k)
    |]

instance SymProg All where
  program ds e = foldl' (flip ($)) e ds

evalAll :: All -> ExpQ
evalAll (All f) = f env0 fenv0 (fromMaybe (error "Zero division"))

dumpAll :: All -> IO ()
dumpAll staged = runQ (pprint <$> evalAll staged) >>= putStrLn
