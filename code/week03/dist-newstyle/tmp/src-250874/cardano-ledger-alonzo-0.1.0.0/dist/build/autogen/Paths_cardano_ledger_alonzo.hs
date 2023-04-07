{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_cardano_ledger_alonzo (
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
bindir     = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/bin"
libdir     = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/lib"
dynlibdir  = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/lib"
datadir    = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/share"
libexecdir = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/libexec"
sysconfdir = "/home/tollan/.cabal/store/ghc-8.10.4.20210212/cardano-ledger-alonzo-0.1.0.0-9bf91b99fdc6b837399ef05687210cc15d83926be9c051c9a4f8dab04d5d9158/etc"

getBinDir     = catchIO (getEnv "cardano_ledger_alonzo_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "cardano_ledger_alonzo_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "cardano_ledger_alonzo_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "cardano_ledger_alonzo_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "cardano_ledger_alonzo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "cardano_ledger_alonzo_sysconfdir") (\_ -> return sysconfdir)




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
