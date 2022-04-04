{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Copyright: (c) 2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Custom Setup to automate package modules discovery
-}

module Autopack
    ( defaultMainAutoModules
    ) where

import Data.Functor ((<&>))
import Data.List (intercalate, nub, stripPrefix)
import Data.Maybe (maybeToList)
import Distribution.ModuleName (ModuleName, fromString)
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
#if MIN_VERSION_Cabal(3,6,0)
import Distribution.Utils.Path (getSymbolicPath)
#endif
import System.Directory.Recursive (getDirRecursive)
import System.FilePath (dropExtension, splitDirectories, takeExtension)

{- | The main function that should be used in the custom @Setup.hs@ files
in the following way:

@
__import__ "Autopack" ('defaultMainAutoModules')

main :: 'IO' ()
main = 'defaultMainAutoModules'
@

This function uses custom hooks with 'defaultMainWithHooks' that
discover all Haskell modules in the @hs-source-dirs@ directories and implies
this list into @exposed-modules@ of the library.
-}
defaultMainAutoModules :: IO ()
defaultMainAutoModules = defaultMainWithHooks $
    modulesHooks getModules simpleUserHooks
  where
    getModules :: GenericPackageDescription -> IO [ModuleName]
    getModules pkgDescr = do
#if MIN_VERSION_Cabal(3,6,0)
        let dirs = fmap getSymbolicPath $
                concatMap (hsSourceDirs . libBuildInfo . condTreeData) $
                maybeToList $ condLibrary pkgDescr
#else
        let dirs = concatMap (hsSourceDirs . libBuildInfo . condTreeData) $
                maybeToList $ condLibrary pkgDescr
#endif
        files <- concat <$> mapM getDirRecursive dirs
        let hsExts = [".hs", ".hsc"]
        let hsFiles = filter (\(takeExtension -> ext) -> ext `elem` hsExts) files
        pure $ map (toModuleName dirs) hsFiles

    toModuleName :: [FilePath] -> FilePath -> ModuleName
    toModuleName dirs file = do
        -- strip dir
        let strippedFile = removeDirPrefix dirs
        -- remove extension
        let noExtFile = dropExtension strippedFile
        -- replace '/' with '.'
        fromString $ intercalate "." $ splitDirectories noExtFile
      where
        removeDirPrefix :: [FilePath] -> FilePath
        removeDirPrefix [] = file
        removeDirPrefix (d:ds) = case stripPrefix d file of
            Just newFile -> drop 1 newFile
            Nothing      -> removeDirPrefix ds

modulesHooks
    :: (GenericPackageDescription -> IO [ModuleName])
    -> UserHooks
    -> UserHooks
modulesHooks getModules hooks = hooks
    { confHook = \(gPackDescr, hBuildInfo) flags -> do
        modules <- getModules gPackDescr
        let newGPackDescr = gPackDescr
                { condLibrary = condLibrary gPackDescr <&> \condLib -> condLib
                    { condTreeData = (condTreeData condLib)
                        { exposedModules = nub $
                            exposedModules (condTreeData condLib) ++ modules
                        }
                    }
                }
        confHook hooks (newGPackDescr, hBuildInfo) flags
    }
