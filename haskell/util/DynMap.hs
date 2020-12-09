module DynMap where

import Data.Dynamic
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map.Strict as Map

newtype DynMap = DynMap { dynMapMap :: Map String Dynamic }

type HasDyns = (?dyns :: DynMap)

getDyn :: (Typeable a, ?dyns :: DynMap) => String -> a -> a
getDyn k def = fromMaybe def (fromDynamic =<< Map.lookup k (dynMapMap ?dyns))

withDynMap :: ((?dyns :: DynMap) => r) -> DynMap -> r
withDynMap cont dm = let ?dyns = dm in cont

emptyDynMap :: DynMap
emptyDynMap = DynMap Map.empty

addDyn :: Typeable a => String -> a -> DynMap -> DynMap
addDyn k v (DynMap m) = DynMap (Map.insert k (toDyn v) m)
