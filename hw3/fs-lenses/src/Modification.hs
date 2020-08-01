{-# LANGUAGE Rank2Types #-}

module Modification
  ( descendantNames
  , getPath
  , move
  , removeEmptySubdir
  , replaceExt
  )
where

import Lens.Micro (SimpleFold, SimpleGetter, filtered, to, traversed, (%~), (&), (.~), (^.), (^..))
import System.FilePath.Posix (addTrailingPathSeparator, replaceExtension, (</>))

import FS (FS (..))
import Practice (contents, dirFilenames, name, _Dir, _File)

-- | Replace extentions of all files in a directory with the specified one (non-recursively).
replaceExt :: FilePath -> FS -> FS
replaceExt newExt dir =
  dir & contents . traverse . _File . name %~ (flip replaceExtension $ newExt)

-- | Get names of all files and directories recursively.
descendantNames :: FS -> [FilePath]
descendantNames fs = subdirNames ++ files ++ concatMap descendantNames subdirs where
  files       = dirFilenames fs
  subdirs     = fs ^.. contents . traversed . _Dir
  subdirNames = subdirs ^.. traversed . name

-- | Remove the specified subdirectory, if empty, or do nothing.
removeEmptySubdir :: FilePath -> FS -> FS
removeEmptySubdir dirName fs =
  fs & contents %~ (filter (not . matchingEmptyDir)) where
  matchingEmptyDir :: FS -> Bool
  matchingEmptyDir (Dir n []) = n == dirName
  matchingEmptyDir _          = False

-- | Enter a subdirectory to narrow down query for full path.
move :: FilePath -> SimpleFold FS FS
move targetName f fs@(Dir dirname elems) =
  let targets   = elems ^.. traversed . filtered ((== targetName) . (^. name))
      prepended = targets & traversed . name %~ (dirname </>)
  in  f & contents . traversed $ fs & contents .~ prepended
move _ _ fs = pure fs

-- | Get full path of the selected filesystem item.
getPath :: SimpleGetter FS FilePath
getPath = to helper where
  helper :: FS -> FilePath
  helper d@(Dir{} ) = addTrailingPathSeparator $ d ^. name
  helper f@(File{}) = f ^. name
