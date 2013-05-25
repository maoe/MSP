module Symantics where
import Prelude hiding (repeat)

class SymExp repr where
  int :: Int -> repr
  var :: String -> repr
  app :: String -> repr -> repr
  add :: repr -> repr -> repr
  sub :: repr -> repr -> repr
  mul :: repr -> repr -> repr
  div :: repr -> repr -> repr
  ifz :: repr -> repr -> repr -> repr

class SymDecl repr where
  declaration :: String -> String -> repr -> repr -> repr

class SymProg repr where
  program :: [repr -> repr] -> repr -> repr

env0 :: a -> b
env0 _ = error "Yikes"

fenv0 :: a -> b
fenv0 = env0

ext :: Eq a => (a -> b) -> a -> b -> a -> b
ext env x v y = if x == y then v else env y

fact :: (SymExp repr, SymDecl repr) => repr -> repr
fact = declaration "fact" "x" $
  ifz (var "x")
    (int 1)
    (mul (var "x") (app "fact" (sub (var "x") (int 1))))

fact15 :: (SymExp repr, SymDecl repr, SymProg repr) => repr
fact15 = program [fact] $ app "fact" (int 15)

repeat :: Int -> (a -> a) -> a -> a
repeat 0 f = f
repeat n f = f . repeat (n-1) f
