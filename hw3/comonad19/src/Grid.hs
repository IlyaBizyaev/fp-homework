{-# LANGUAGE InstanceSigs #-}

module Grid
  ( Grid(..)
  , up
  , down
  , left
  , right
  , gridWrite
  )
where

import Control.Comonad (Comonad (..), extract)

import ListZipper (ListZipper (..), genericMove, listLeft, listRight, listWrite)

-- | 2D combination of 'ListZipper's
newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }

instance Functor Grid where
  fmap f grid = Grid $ fmap (fmap f) $ unGrid grid

-- | Move the viewport of 'ListZipper' up.
up :: Grid a -> Grid a
up (Grid g) = Grid (listLeft g)

-- | Move the viewport of 'ListZipper' down.
down :: Grid a -> Grid a
down (Grid g) = Grid (listRight g)

-- | Move the viewport of 'ListZipper' to the left.
left :: Grid a -> Grid a
left (Grid g) = Grid (fmap listLeft g)

-- | Move the viewport of 'ListZipper' to the right.
right :: Grid a -> Grid a
right (Grid g) = Grid (fmap listRight g)

-- | Write value to the viewport of 'ListZipper'.
gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
 where
  oldLine = extract g
  newLine = listWrite x oldLine

-- | Make 'ListZipper' of 'Grid's with viewport moved to left and right.
horizontal :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right

-- | Make 'ListZipper' of 'Grid's with viewport moved up and down.
vertical :: Grid a -> ListZipper (Grid a)
vertical = genericMove up down

instance Comonad Grid where
  extract :: Grid a -> a
  extract (Grid g) = extract $ extract g

  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical
