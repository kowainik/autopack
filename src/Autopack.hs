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
import Data.List (nub, stripPrefix)
import Data.Maybe (maybeToList)
import Distribution.ModuleName (ModuleName, fromComponents)
import Distribution.Simple (UserHooks (..), defaultMainWithHooks, simpleUserHooks)
import Distribution.Types.BuildInfo (BuildInfo (..))
import Distribution.Types.CondTree (CondTree (..))
import Distribution.Types.GenericPackageDescription (GenericPackageDescription (..))
import Distribution.Types.Library (Library (..))
import System.Directory.Recursive (getDirRecursive)
import System.FilePath (dropExtension, splitDirectories, takeExtension)


defaultMainAutoModules :: IO ()
defaultMainAutoModules = defaultMainWithHooks $
    modulesHooks getModules simpleUserHooks
  where
    getModules :: GenericPackageDescription -> IO [ModuleName]
    getModules pkgDescr = do
        let dirs = concatMap (hsSourceDirs . libBuildInfo . condTreeData) $
                maybeToList $ condLibrary pkgDescr
        files <- concat <$> mapM getDirRecursive dirs
        print files
        let hsFiles = filter (\f -> takeExtension f == ".hs") files
        pure $ map (toModuleName dirs) hsFiles

    toModuleName :: [FilePath] -> FilePath -> ModuleName
    toModuleName dirs file = do
        -- strip dir
        let strippedFile = removeDirPrefix dirs
        -- remove extension
        let noExtFile = dropExtension strippedFile
        -- replace '/' with '.'
        fromComponents $ splitDirectories noExtFile
      where
        removeDirPrefix :: [FilePath] -> FilePath
        removeDirPrefix [] = file
        removeDirPrefix (d:ds) = case stripPrefix d file of
            Just newFile ->  drop 1 newFile
            Nothing      -> removeDirPrefix ds

modulesHooks
    :: (GenericPackageDescription -> IO [ModuleName])
    -> UserHooks
    -> UserHooks
modulesHooks getModules hooks = hooks
    { confHook = \(gPackDescr, hBuildInfo) flags -> do
          modules <- getModules gPackDescr
          print modules
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
