module PagedVector where

import Control.Monad
import Data.Maybe

import Control.Lens
import Data.Vector (Vector)
import Data.Vector.Generic ((!), (//))
import qualified Data.Vector.Generic as Vector
import qualified Data.Vector.Generic.Mutable as MVector

data Paged vec a = Paged { pageSize :: Int, pageDefault :: a, pages :: Vector (vec a)}

atAddr :: (Eq a, Monoid (vec a), Vector.Vector vec a) => Int -> Lens' (Paged vec a) a
atAddr p
  | p < 0 = error $ "invalid address " <> show p <> ", must not be negative"
  | otherwise = \f mem@Paged{pageSize, pageDefault, pages} -> let
    (q, r) = p `divMod` pageSize
    in if q >= length pages
        then f pageDefault <&> \x ->
          if x == pageDefault
            then mem
            else let
              filler = Vector.replicate (q - length pages - 1) Vector.empty
              final = Vector.replicate pageSize pageDefault // [(r, x)]
              in mem{pages = pages <> filler <> Vector.singleton final}
        else let
          page = pages Vector.! q
          input = if Vector.length page > 0 then page ! r else pageDefault
          page' = if Vector.length page > 0 then page else Vector.replicate pageSize pageDefault
          in f input <&> \x ->
              if x == input
                then mem
                else mem{ pages = pages // [(q, page' // [(r, x)])] }

instance (Show a, Show (vec a), Vector.Vector vec a) => Show (Paged vec a) where
  showsPrec p Paged{pageSize, pageDefault, pages} = showParen (p > 10) $
    showString "PagedVector.fromList"
    . showsPrec 11 pageSize
    . showString " "
    . showsPrec 11 pageDefault
    . showString " "
    . showList
      [ if Vector.length page > 0
          then Vector.toList page
          else replicate pageSize pageDefault
      | page <- Vector.toList pages
      ]

instance (Eq a, Eq (vec a), Vector.Vector vec a) => Eq (Paged vec a) where
  x == y =
    pageSize x == pageSize y
    && pageDefault x == pageDefault y
    && let
      xpages = pages x
      ypages = pages y
      def = pageDefault x
      blank = Vector.all (==def)
      minLength = min (Vector.length xpages) (Vector.length ypages)
      excess = Vector.drop minLength xpages Vector.++ Vector.drop minLength ypages
      in (Vector.length xpages == Vector.length ypages
      || (Vector.all blank excess))
      && isJust (Vector.zipWithM_
          (\r s -> guard $
            (blank r && blank s)
            || r == s
            )
          xpages ypages)

mkPaged :: Vector.Vector vec a => Int -> a -> vec a -> Paged vec a
mkPaged pageSize def xs = let
  (q, r) = Vector.length xs `divMod` pageSize
  chunks = if r == 0
    then Vector.generate q \i -> let
      start = pageSize * i
      in Vector.slice start pageSize xs
    else Vector.generate (q+1) \i -> let
      start = pageSize * i
      in if i /= q
          then Vector.slice start pageSize xs
          else Vector.create
            do
              v <- MVector.new pageSize
              let (vhead, vtail) = MVector.splitAt r v
              Vector.copy vhead (Vector.slice start r xs)
              MVector.set vtail def
              pure v
  in Paged pageSize def chunks
