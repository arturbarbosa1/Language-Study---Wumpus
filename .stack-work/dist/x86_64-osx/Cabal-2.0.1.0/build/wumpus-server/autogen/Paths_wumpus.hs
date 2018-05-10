{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_wumpus (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/bin"
libdir     = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/lib/x86_64-osx-ghc-8.2.2/wumpus-0.1.0.0-GU7Iol1Olj94qROBFQyUQh-wumpus-server"
dynlibdir  = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/lib/x86_64-osx-ghc-8.2.2"
datadir    = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/share/x86_64-osx-ghc-8.2.2/wumpus-0.1.0.0"
libexecdir = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/libexec/x86_64-osx-ghc-8.2.2/wumpus-0.1.0.0"
sysconfdir = "/Users/Barbosa1/Desktop/wumpus/.stack-work/install/x86_64-osx/lts-11.8/8.2.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "wumpus_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "wumpus_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "wumpus_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "wumpus_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "wumpus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "wumpus_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
