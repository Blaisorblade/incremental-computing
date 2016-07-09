module Data.MultiChange (

    -- * Type

    MultiChange,

    -- * Construction

    singleton,
    fromList,

    -- * Monad structure

    mapMultiChange,
    returnMultiChange,
    joinMultiChange,
    bindMultiChange,

    -- * Multi composition

    compose,
    composeMap

) where

-- Prelude

import           Prelude hiding (id, (.))
import qualified Prelude

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

-- * Monad structure. We use the Trans category rather than Hask.

mapMultiChange :: Trans p q -> Trans (MultiChange p) (MultiChange q)
mapMultiChange trans = stTrans (\ val -> do
    ~(val', prop) <- toSTProc trans val
    let multiProp change = do
            atomics' <- mapM prop (toList change)
            Prelude.return (fromList atomics')
    Prelude.return (val', multiProp))

returnMultiChange :: Trans p (MultiChange p)
returnMultiChange = simpleTrans id singleton

joinMultiChange :: Trans (MultiChange (MultiChange p)) (MultiChange p)
joinMultiChange = compose

bindMultiChange :: Trans p (MultiChange q) -> Trans (MultiChange p) (MultiChange q)
bindMultiChange = composeMap

-- * Multi composition

compose :: Monoid p => Trans (MultiChange p) p
compose = simpleTrans id (mconcat . reverse . toList)
{-FIXME:
    Check whether the use of mconcat . reverse is questionable regarding space
    usage or strictness. If it is, consider using foldr (flip mappend) mempty
    instead.
-}

composeMap :: Monoid q => Trans p q -> Trans (MultiChange p) q
composeMap trans = compose . mapMultiChange trans
