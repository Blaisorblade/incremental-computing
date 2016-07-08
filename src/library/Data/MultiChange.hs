module Data.MultiChange (

    -- * Type

    MultiChange,

    -- * Construction

    singleton,
    fromList,

    -- * Monad structure

    map,
    return,
    join,
    bind,

    -- * Multi composition

    compose,
    composeMap

) where

-- Prelude

import           Prelude hiding (id, (.), map, return)
import qualified Prelude
{-FIXME:
    After establishment of the Applicative–Monad proposal, we have to optionally
    hide join.
-}

-- Control

import Control.Category
import Control.Arrow (second)
import Control.Monad (liftM)

-- Data

import           Data.Monoid
import           Data.Foldable as Foldable
import qualified Data.List as List
import           Data.DList (DList)
import qualified Data.DList as DList
import           Data.Incremental

-- * Type

newtype MultiChange p = MultiChange (Dual (DList p)) deriving Monoid

instance Show p => Show (MultiChange p) where

    showsPrec prec xs = showParen (prec > 10) $
                        showString "fromList " . shows (toList xs)
    -- NOTE: This is basically taken from Data.Sequence.

instance Read p => Read (MultiChange p) where

    readsPrec prec = readParen (prec > 10) $ \ str -> do
        ("fromList", rest) <- lex str
        (list, rest') <- reads rest
        Prelude.return (fromList list, rest')
    -- NOTE: This is basically taken from Data.Sequence.

instance Foldable MultiChange where

    foldMap fun (MultiChange (Dual dList)) = foldMap fun dList

    foldr next init (MultiChange (Dual dList)) = Foldable.foldr next init dList

instance Change p => Change (MultiChange p) where

    type Value (MultiChange p) = Value p

    change $$ val = List.foldl' (flip ($$)) val (toList change)

-- * Construction

singleton :: p -> MultiChange p
singleton = MultiChange . Dual . DList.singleton

{-NOTE:
    The lists are “in diagramatic order” (first atomic change at the beginning).
-}

fromList :: [p] -> MultiChange p
fromList = MultiChange . Dual . DList.fromList

-- * Monad structure

map :: Trans p q -> Trans (MultiChange p) (MultiChange q)
map trans = stTrans (\ val -> do
    ~(val', prop) <- toSTProc trans val
    let multiProp change = do
            atomics' <- mapM prop (toList change)
            Prelude.return (fromList atomics')
    Prelude.return (val', multiProp))

return :: Trans p (MultiChange p)
return = simpleTrans id singleton

join :: Trans (MultiChange (MultiChange p)) (MultiChange p)
join = compose

bind :: Trans p (MultiChange q) -> Trans (MultiChange p) (MultiChange q)
bind = composeMap

-- * Multi composition

compose :: Monoid p => Trans (MultiChange p) p
compose = simpleTrans id (mconcat . reverse . toList)
{-FIXME:
    Check whether the use of mconcat . reverse is questionable regarding space
    usage or strictness. If it is, consider using foldr (flip mappend) mempty
    instead.
-}

composeMap :: Monoid q => Trans p q -> Trans (MultiChange p) q
composeMap trans = compose . map trans