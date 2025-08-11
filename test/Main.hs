module Main where

newtype W a b = W (a, b) deriving newtype (Show, Eq, Functor, Applicative)

eg :: W [String] Int
eg = ADo.do
  a <- W (["A"], 1 :: Int)
  W (["X"], ())
  b <- W (["B"], 2 :: Int)
  let e = a + b
  c <- W (["C"], 3 :: Int)
  pure (a + b + e + c)

main :: IO ()
main = print eg -- (["A", "X", "B", "C"], 9)

-- $> main
