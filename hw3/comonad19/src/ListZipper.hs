{-# LANGUAGE InstanceSigs #-}

module ListZipper
  ( ListZipper(..)
  , genericMove
  , iterateTail
  , listLeft
  , listRight
  , listWrite
  , toList
  )
where

import Control.Comonad (Comonad (..))

-- | Data type representing a list that allows to focus on a single element.
data ListZipper a = LZ [a] a [a]

instance Functor ListZipper where
  fmap f (LZ ls x rs) = LZ (fmap f ls) (f x) (fmap f rs)

-- | Shift 'ListZipper' viewport to the left.
listLeft :: ListZipper a -> ListZipper a
listLeft (LZ (a : as) x bs) = LZ as a (x : bs)
listLeft _                  = error "listLeft"

-- | Shift 'ListZipper' viewport to the right.
listRight :: ListZipper a -> ListZipper a
listRight (LZ as x (b : bs)) = LZ (x : as) b bs
listRight _                  = error "listRight"

-- | Like 'iterate', but without the initial value.
iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

-- | Given 2 functions to produce elements, generate 'ListZipper' from the viewport element.
genericMove :: (a -> a) -> (a -> a) -> a -> ListZipper a
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x

  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight

-- | Write to the 'ListZipper' viewport.
listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

-- | Convert 'ListZipper' to a list with the specified threshold.
toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs
