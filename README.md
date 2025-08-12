# "ado-plug"

Use do-notation with applicative functors.

```hs
{-# OPTIONS_GHC -fplugin=ADo #-}
{-# LANGUAGE QualifiedDo #-}
module Example where

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
```

Check test/Main.hs for more examples.


### What are errors like?

Should be reasonable.

```hs
eg0 = ADo.do
  a <- pure 0
  b <- pure a
  b
```

```hs
    Variable not in scope: a :: Int
   |
17 |   b <- pure a
   |             ^
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

