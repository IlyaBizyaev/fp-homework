module TestData
  ( sampleFS
  , rootFileFS
  )
where

import FS (FS (..))

-- | Sample directory, used as test data.
sampleFS :: FS
sampleFS = Dir
  { _name     = "sample"
  , _contents =
    [ Dir { _name = "first", _contents = [File { _name = "42.txt" }] }
    , Dir
      { _name     = "seco.nd"
      , _contents = [ Dir { _name     = "third"
                          , _contents = [File { _name = "virus.exe" }]
                          }
                    ]
      }
    , Dir { _name = "empty", _contents = [] }
    , File { _name = "README.md" }
    , File { _name = "LICENSE" }
    ]
  }

-- | Sample file, used as test data.
rootFileFS :: FS
rootFileFS = File { _name = "rootFile" }
