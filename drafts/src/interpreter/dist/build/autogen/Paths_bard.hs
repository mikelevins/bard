module Paths_bard (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,9], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/bard-0.9/ghc-7.0.2"
datadir    = "/usr/local/share/bard-0.9"
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
