module Practice
  ( _File
  , _Dir
  , addRootNameSuffix
  , contents
  , dirChildren
  , dirFilenames
  , dirName
  , fileName
  , firstSubdirectory
  , name
  , renameToRoot
  )
where

import Lens.Micro (Lens', Traversal', lens, traversed, (&), (.~), (<>~), (^.), (^..), (^?))

import FS (FS (..))

-- | Traverse FS if it is a file.
_File :: Traversal' FS FS
_File _ fs@(Dir _ _) = pure fs
_File f file         = f file

-- | Traverse FS if it is a directory.
_Dir :: Traversal' FS FS
_Dir _ fs@(File _) = pure fs
_Dir f d           = f d

-- | Get or set name of a filesystem item.
name :: Lens' FS FilePath
name = lens _name (\fs newName -> fs { _name = newName })

-- | Traverse contents of a directory.
contents :: Traversal' FS [FS]
contents _ fs@(File _ ) = pure fs
contents f (   Dir n c) = (Dir n) <$> f c

-- | Get contents of a directory as a list.
dirChildren :: FS -> [FS]
dirChildren fs = fs ^. contents

-- | Get name of a directory, or Nothing for a file.
dirName :: FS -> Maybe FilePath
dirName fs = fs ^? _Dir . name

-- | Get name of a file, or Nothing for a directory.
fileName :: FS -> FilePath
fileName fs = fs ^. _File . name

-- | Change name of the root filesystem item to "/".
renameToRoot :: FS -> FS
renameToRoot fs = fs & name .~ "/"

-- | Add a suffix to the name of the root filesystem item.
addRootNameSuffix :: FilePath -> FS -> FS
addRootNameSuffix suffix fs = fs & name <>~ suffix

-- | Get the name of the first subdirectory of a directory, or Nothing, if none.
firstSubdirectory :: FS -> Maybe FilePath
firstSubdirectory fs = fs ^? contents . traversed . _Dir . name

-- | Get names of all files in a directory as a list.
dirFilenames :: FS -> [FilePath]
dirFilenames fs = fs ^.. contents . traversed . _File . name
