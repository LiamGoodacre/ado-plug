# "ado-plug"

Use do-notation with applicative functors.

```hs
{-# OPTIONS_GHC -fplugin=ADo #-}
{-# LANGUAGE QualifiedDo #-}
module Main where

newtype W a b = W (a, b) deriving newtype (Show, Eq, Functor, Applicative)

eg :: W [String] Int
eg = ADo.do
  a <- W (["A"], 1 :: Int)
  W (["X"], ())
  b <- W (["B"], 2 :: Int)
  let e = a + b
  c <- W (["C"], 3 :: Int)
  a + b + e + c

main :: IO ()
main = print eg -- (["A", "X", "B", "C"], 9)

-- $> main
```


### Can I use it?

I haven't released it anywhere, nor made it work with various GHC versions.


### Doesn't ApplicativeDo already do this?

`ApplicativeDo` is a GHC extension that globally mangles do-notation.
Approximately, it tries to transform do-notation into applicative style
wherever possible, but also leave binds alone if it's (syntactically) obvious
that they're needed. However, it is very buggy and can cause compilation errors
on previously working do blocks.


### What about QualifiedDo?

`QualifiedDo` is a GHC extension that allows you to use do-notation in which
the bind/ap/etc operators come from a qualified import. This doesn't let you
implement ado.

