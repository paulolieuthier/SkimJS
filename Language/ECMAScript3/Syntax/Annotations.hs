-- | A few helpers to work with the AST annotations
module Language.ECMAScript3.Syntax.Annotations where

import Language.ECMAScript3.Syntax
import Data.Traversable
import Control.Applicative
import Control.Arrow
import Control.Monad.State hiding (mapM)
import Prelude hiding (mapM)

-- | Removes annotations from a tree
removeAnnotations :: Traversable t => t a -> t ()
removeAnnotations = reannotate (const ())

-- | Changes all the labels in the tree to another one, given by a
-- function.
reannotate :: Traversable t => (a -> b) -> t a -> t b
reannotate f tree = traverse (pure . f) tree ()

-- | add an extra field to the AST labels (the label would look like @
-- (a, b) @)
addExtraAnnotationField :: Traversable t => b -> t a -> t (a, b)
addExtraAnnotationField def t = traverse (\z -> pure (z, def)) t ()

-- | remove an extra field
removeExtraAnnotationField :: Traversable t => t (a, b) -> t a
removeExtraAnnotationField t = traverse (pure . fst) t ()


-- | Assigns unique numeric (Int) ids to each node in the AST. Returns
-- a pair: the tree annotated with UID's and the last ID that was
-- assigned.
assignUniqueIds :: Traversable t => Int -- ^ starting id
                                 -> t a -- ^ tree root
                                 -> (t (a, Int), Int) 
assignUniqueIds first tree =
  (returnA *** \i -> i-1) $ runState (mapM f tree) first
  where f :: a -> State Int (a, Int)
        f a = do i <- get
                 put (i+1)
                 return (a, i)
