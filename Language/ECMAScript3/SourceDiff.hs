-- | Simple textual diffing of JavaScript programs for inspecting test
-- failures
module Language.ECMAScript3.SourceDiff where

import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
import Language.ECMAScript3.Syntax
import Language.ECMAScript3.PrettyPrint
import Data.List (intersperse, intercalate)

jsDiff :: JavaScript a -> JavaScript a -> String
jsDiff js1 js2 = 
  let plines = lines . show . prettyPrint
      diff = getGroupedDiff (plines js1) (plines js2)
  in ppDiff diff
