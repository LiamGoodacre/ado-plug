module ADo (plugin, (>>=)) where

import Control.Monad.IO.Class (liftIO)
import Data.Function ((&))
import Data.Generics (everywhereM, mkM)
import GHC.Builtin.Names
  ( ap_RDR,
    fmap_RDR,
    pure_RDR,
  )
import GHC.Hs
  ( ExprLStmt,
    GhcPs,
    HsDoFlavour (DoExpr),
    HsExpr (HsDo),
    HsModule,
    HsParsedModule (hpm_module),
    LHsExpr,
    LPat,
    StmtLR (BindStmt, BodyStmt),
    mkHsLam,
    nlHsApp,
    nlHsVar,
    nlWildPat,
    noAnn,
  )
import GHC.Plugins
  ( GenLocated (L),
    Hsc,
    Located,
    ModuleName (ModuleName),
    ParsedResult (parsedResultModule),
    Plugin (parsedResultAction),
    defaultPlugin,
  )
import Prelude

mkPure :: LHsExpr GhcPs -> LHsExpr GhcPs
mkPure = nlHsApp (nlHsVar pure_RDR)

mkAp :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
mkAp mf mx = nlHsApp (nlHsApp (nlHsVar ap_RDR) mf) mx

mkFmap :: LHsExpr GhcPs -> LHsExpr GhcPs -> LHsExpr GhcPs
mkFmap mf mx = nlHsApp (nlHsApp (nlHsVar fmap_RDR) mf) mx

mkApN :: LHsExpr GhcPs -> [LHsExpr GhcPs] -> LHsExpr GhcPs
mkApN body [] = mkPure body
mkApN body [mone] = mkFmap body mone
mkApN body margs = foldl' mkAp (mkPure body) margs

mkDo :: [ExprLStmt GhcPs] -> ExprLStmt GhcPs -> LHsExpr GhcPs
mkDo stmts finalStmt =
  L noAnn (HsDo noAnn (DoExpr Nothing) (L noAnn (stmts ++ [finalStmt])))

rewriteApplicatively :: [ExprLStmt GhcPs] -> Hsc (LHsExpr GhcPs)
rewriteApplicatively [] = liftIO $ fail "rewriteApplicatively: empty stmts"
rewriteApplicatively stmts = do
  let fnPatterns :: [LPat GhcPs]
      apArgs :: [LHsExpr GhcPs]
      otherStmts :: [ExprLStmt GhcPs]
      (fnPatterns, apArgs, otherStmts) =
        init stmts & foldMap \lstmt@(L _ stmt) -> case stmt of
          BindStmt _ bindPat bindExpr -> ([bindPat], [bindExpr], [])
          BodyStmt _ bindExpr _ _ -> ([nlWildPat], [bindExpr], [])
          _ -> ([], [], [lstmt])

  let strippedFinalStmt :: ExprLStmt GhcPs
      strippedFinalStmt = last stmts

  let apFn :: LHsExpr GhcPs
      apFn = mkHsLam (L noAnn fnPatterns) (mkDo otherStmts strippedFinalStmt)

  pure $ mkApN apFn apArgs
{-# INLINE rewriteApplicatively #-}

rewriteADos :: LHsExpr GhcPs -> Hsc (LHsExpr GhcPs)
rewriteADos (L _ (HsDo _ (DoExpr (Just (ModuleName "ADo"))) (L _ stmts))) = rewriteApplicatively stmts
rewriteADos expr = pure expr
{-# INLINE rewriteADos #-}

on_hpm_module ::
  (Functor m) =>
  (Located (HsModule GhcPs) -> m (Located (HsModule GhcPs))) ->
  (HsParsedModule -> m HsParsedModule)
on_hpm_module f b = (\m -> b {hpm_module = m}) <$> f (hpm_module b)
{-# INLINE on_hpm_module #-}

onParsedResultModule ::
  (Functor m) =>
  (HsParsedModule -> m HsParsedModule) ->
  (ParsedResult -> m ParsedResult)
onParsedResultModule f b = (\m -> b {parsedResultModule = m}) <$> f (parsedResultModule b)
{-# INLINE onParsedResultModule #-}

plugin :: Plugin
plugin =
  defaultPlugin
    { parsedResultAction = \_ _ ->
        onParsedResultModule (on_hpm_module (everywhereM (mkM rewriteADos)))
    }
