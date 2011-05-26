module Paths_bard (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,8], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/bard-0.8/ghc-6.12.3"
datadir    = "/usr/local/share/bard-0.8"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "bard_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "bard_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "bard_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "bard_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
