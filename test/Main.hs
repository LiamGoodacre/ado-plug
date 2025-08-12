module Main where

egPure :: (Applicative m) => m ()
egPure = ADo.do ()

egFmap :: (Functor m) => m x -> m (x, x)
egFmap src = ADo.do
  x <- src
  (x, x)

egAp :: (Applicative m) => m x -> m (x, x)
egAp src = ADo.do
  x <- src
  y <- src
  (x, y)

egLet :: (Applicative m) => m x -> m (x, x)
egLet src = ADo.do
  let xx = (x, x)
  let yy = (y, y)
  x <- src
  y <- src
  (fst xx, snd yy)

newtype W a b = W (a, b) deriving newtype (Show, Eq, Functor, Applicative)

egW :: W [String] Int
egW = ADo.do
  a <- W (["A"], 1 :: Int)
  W (["X"], ())
  b <- W (["B"], 2 :: Int)
  let e = a + b
  c <- W (["C"], 3 :: Int)
  a + b + e + c

main :: IO ()
main = print egW -- (["A", "X", "B", "C"], 9)

-- $> main
