{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_cis194 (
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

bindir     = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/bin"
libdir     = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/lib/x86_64-osx-ghc-8.6.4/cis194-0.1.0.0-4Q1MQXHuO6xEeR2CN5wgdR-cis194"
dynlibdir  = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/lib/x86_64-osx-ghc-8.6.4"
datadir    = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/share/x86_64-osx-ghc-8.6.4/cis194-0.1.0.0"
libexecdir = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/libexec/x86_64-osx-ghc-8.6.4/cis194-0.1.0.0"
sysconfdir = "/Users/maxyaknov/Projects/cis194/.stack-work/install/x86_64-osx/lts-13.16/8.6.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "cis194_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "cis194_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "cis194_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "cis194_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cis194_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cis194_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
