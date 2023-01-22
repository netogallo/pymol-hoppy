{-# LANGUAGE TemplateHaskell #-}
module Data.Pymol.Extra.TemplateHaskell
  ( simple
  ) where


import           Language.Haskell.TH.Syntax (Dec (DataD), Name, Q, Type(ConT), newName, Con (NormalC), SourceStrictness (SourceLazy, NoSourceStrictness), Bang (Bang), SourceUnpackedness (NoSourceUnpackedness))

-- simple :: Qutoe q => Int -> q [Decl]
simple :: Name -> Q [Dec]
simple cls = do
  name <- newName "Kaiser"
  name' <- newName "Kaiser"
  pure [DataD [] name [] Nothing [NormalC name' [(Bang NoSourceUnpackedness NoSourceStrictness, ConT cls)]] []]