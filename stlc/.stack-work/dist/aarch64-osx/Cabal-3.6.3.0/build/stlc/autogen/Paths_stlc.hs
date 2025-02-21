{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_stlc (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/bin"
libdir     = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/lib/aarch64-osx-ghc-9.2.5/stlc-0.1.0.0-6JHsq5WcAT2LSt6ejmLRbY-stlc"
dynlibdir  = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/lib/aarch64-osx-ghc-9.2.5"
datadir    = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/share/aarch64-osx-ghc-9.2.5/stlc-0.1.0.0"
libexecdir = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/libexec/aarch64-osx-ghc-9.2.5/stlc-0.1.0.0"
sysconfdir = "/Users/petroskitazos/dev/elixir-mpst/stlc/.stack-work/install/aarch64-osx/13ce87fb2ac2f14f150aae6ccd3a8528b34a5884eb3c945dc3f8d3c3942c97c2/9.2.5/etc"

getBinDir     = catchIO (getEnv "stlc_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "stlc_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "stlc_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "stlc_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "stlc_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "stlc_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
