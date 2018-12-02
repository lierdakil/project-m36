{-# LANGUAGE PatternSynonyms #-}
module TutorialD.QQ (tutdrel, tutdctx, tutdctxio) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import ProjectM36.Base
import qualified Data.Text as T
import TutorialD.Interpreter.Base
import TutorialD.Interpreter.DatabaseContextExpr
import TutorialD.Interpreter.RelationalExpr
import TutorialD.Interpreter.DatabaseContextIOOperator
import TutorialD.THOrphans ()

tutdrel, tutdctx, tutdctxio :: QuasiQuoter
tutdrel = mkQQ (relExprP :: Parser RelationalExpr)
tutdctx = mkQQ databaseContextExprP
tutdctxio = mkQQ dbContextIOExprP

mkQQ :: (Lift t) => Parser t -> QuasiQuoter
mkQQ p = QuasiQuoter {
    quoteExp = either (fail . filterNewlines . parseErrorPretty) lift . parse (
      p <* eof
      ) "" . T.pack . escapeQQEnd
  , quotePat = fail "This QuasiQuoter can not be used as pattern"
  , quoteType = fail "This QuasiQuoter can not be used as type"
  , quoteDec = fail "This QuasiQuoter can not be used as declaration"
  }

escapeQQEnd :: String -> String
escapeQQEnd ('|':QQEscapeChar:s)
  | (bs, ']':rest) <- span (== QQEscapeChar) s = '|' : bs ++ ']' : escapeQQEnd rest
escapeQQEnd (x : s) = x : escapeQQEnd s
escapeQQEnd [] = []

pattern QQEscapeChar :: Char
pattern QQEscapeChar = '~'

filterNewlines :: String -> String
filterNewlines (x:xs) = (if x == '\n' then ' ' else x) : filterNewlines xs
filterNewlines [] = []
