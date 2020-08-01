{-# LANGUAGE InstanceSigs #-}

module FS
  ( FS(..)
  , getDirectory
  )
where

import Control.Exception (throwIO)
import Data.List (intercalate)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath.Posix (dropTrailingPathSeparator, takeFileName, (</>))

-- | Record data type for filesystem structure representation.
data FS
    = Dir
    { _name     :: FilePath
    , _contents :: [FS]
    }
    | File
    { _name     :: FilePath
    } deriving (Eq)

instance Show FS where
  show :: FS -> String
  show fs = showIndented 0 fs   where
    indentBy :: Int -> String -> String
    indentBy dep str = (replicate (dep * 2) ' ') ++ str
    showIndented :: Int -> FS -> String
    showIndented dep (File n) = indentBy dep n
    showIndented dep (Dir n c) =
      intercalate "\n" ((indentBy dep n) : map (showIndented (dep + 1)) c)

-- | Read structure of the specified directory into a virtual representation.
getDirectory :: FilePath -> IO FS
getDirectory dirPath = do
  fsItem <- getFSItem dirPath
  case fsItem of
    File _ ->
      throwIO $ userError "Specified path does not point at a directory"
    dir -> return dir

-- | Read structure of the specified filesystem item into a virtual representation.
getFSItem :: FilePath -> IO FS
getFSItem itemPath = do
  let itemName = takeFileName . dropTrailingPathSeparator $ itemPath
  isDir <- doesDirectoryExist itemPath
  if isDir
    then do
      items       <- listDirectory itemPath
      dirContents <- mapM (getFSItem . (itemPath </>)) items
      return $ Dir { _name = itemName, _contents = dirContents }
    else do
      return $ File { _name = itemName }
