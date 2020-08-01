{-# LANGUAGE Rank2Types #-}

module Traversal
  ( cd
  , file
  , ls
  )
where

import Lens.Micro (Traversal', filtered, traversed, (^.))

import FS (FS (..))
import Practice (contents, name, _Dir, _File)

-- | View the specified subdirectory.
cd :: FilePath -> Traversal' FS FS
cd dirName = _Dir . contents . traversed . _Dir . (filtered p) where
  p :: FS -> Bool
  p d@(Dir{}) = d ^. name == dirName
  p _         = False

-- | List contents of the currently viewed directory.
ls :: Traversal' FS FilePath
ls = _Dir . contents . traversed . name

-- | Return the name of the specified file, if exists, or return Nothing.
file :: FilePath -> Traversal' FS FilePath
file fileName = _Dir . contents . traversed . _File . (filtered p) . name where
  p :: FS -> Bool
  p f@(File{}) = f ^. name == fileName
  p _          = False
