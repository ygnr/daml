-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

module DAML.Assistant.Env
    ( Env (..)
    , DamlPath (..)
    , ProjectPath (..)
    , SdkPath (..)
    , SdkVersion (..)
    , getMinimalDamlEnv
    , getDamlEnv
    , testDamlEnv
    , getDamlPath
    , getDamlAssistantPath
    , getProjectPath
    , getSdk
    , getDispatchEnv
    ) where

import DAML.Assistant.Types
import DAML.Assistant.Util
import DAML.Assistant.Version
import DAML.Assistant.Install
import DAML.Project.Config
import DAML.Project.Consts hiding (getDamlPath, getProjectPath)
import System.Directory
import System.FilePath
import System.Environment.Blank
import System.IO
import System.Info.Extra
import Control.Monad.Extra
import Control.Exception.Safe
import Data.Maybe
import Data.Either.Extra

-- | Get a minimal environment in which to run daml install.
getMinimalDamlEnv :: IO Env
getMinimalDamlEnv = do
    envDamlPath <- getDamlPath
    envDamlAssistantPath <- getDamlAssistantPath envDamlPath Nothing
    let envDamlAssistantSdkVersion = Nothing
        envProjectPath = Nothing
        envSdkVersion = Nothing
        envSdkPath = Nothing
        envLatestStableSdkVersion = Nothing
    pure Env {..}

-- | Calculate the environment variables in which to run daml commands.
getDamlEnv :: IO Env
getDamlEnv = do
    envDamlPath <- getDamlPath
    envDamlAssistantSdkVersion <- getDamlAssistantSdkVersion
    envDamlAssistantPath <- getDamlAssistantPath envDamlPath envDamlAssistantSdkVersion
    envProjectPath <- getProjectPath
    (envSdkVersion, envSdkPath) <- getSdk envDamlPath
        envDamlAssistantSdkVersion envProjectPath
    envLatestStableSdkVersion <- getLatestStableSdkVersion envDamlPath
    pure Env {..}

-- | (internal) Override function with environment variable
-- if it is available.
overrideWithEnvVar
    :: String                   -- ^ env var name
    -> (String -> t)            -- ^ parser for env var
    -> IO t                     -- ^ calculation to override
    -> IO t
overrideWithEnvVar envVar parse calculate =
    maybeM calculate (pure . parse) (getEnv envVar)

-- | (internal) Same as overrideWithEnvVar but accepts "" as
-- Nothing and throws exception on parse failure.
overrideWithEnvVarMaybe
    :: Exception e
    => String                   -- ^ env var name
    -> (String -> Either e t)   -- ^ parser for env var
    -> IO (Maybe t)             -- ^ calculation to override
    -> IO (Maybe t)
overrideWithEnvVarMaybe envVar parse calculate = do
    valueM <- getEnv envVar
    case valueM of
        Nothing -> calculate
        Just "" -> pure Nothing
        Just value ->
            Just <$> requiredE
                ("Invalid value for environment variable " <> pack envVar <> ".")
                (parse value)

-- | Get the latest stable SDK version. Can be overriden with
-- DAML_SDK_LATEST_VERSION environment variable.
getLatestStableSdkVersion :: DamlPath -> IO (Maybe SdkVersion)
getLatestStableSdkVersion damlPath =
    overrideWithEnvVarMaybe sdkVersionLatestEnvVar (parseVersion . pack) $
        getLatestSdkVersionCached damlPath

-- | Determine the viability of running sdk commands in the environment.
-- Returns the first failing test's error message.
testDamlEnv :: Env -> IO (Maybe Text)
testDamlEnv Env{..} = firstJustM (\(test, msg) -> unlessMaybeM test (pure msg))
    [ ( doesDirectoryExist (unwrapDamlPath envDamlPath)
      ,  "The daml home directory does not exist. Please check if DAML_HOME is incorrectly set, "
      <> "or run \n\n    daml install --initial\n\nto create the daml home directory and "
      <> "install the SDK." )
    , ( pure (isJust envSdkVersion)
      ,  "Could not determine SDK version. Please check if DAML_HOME is incorrectly set, or "
      <> "check the daml project config file, or run \"daml install\" to install the latest "
      <> "version of the SDK." )
    , ( maybe (pure False) (doesDirectoryExist . unwrapSdkPath) envSdkPath
      ,  "The DAML SDK directory does not exist. Please check if DAML_SDK or DAML_SDK_VERSION "
      <> "are incorrectly set, or run \"daml install\" to install the appropriate SDK version.")
    , ( maybe (pure True) (doesDirectoryExist . unwrapProjectPath) envProjectPath
      , "The project directory does not exist. Please check if DAML_PROJECT is incorrectly set.")
    ]

-- | Determine the absolute path to the assistant. Can be overriden with
-- DAML_ASSISTANT env var.
getDamlAssistantPath :: DamlPath -> Maybe DamlAssistantSdkVersion -> IO DamlAssistantPath
getDamlAssistantPath damlPath damlVersion =
    overrideWithEnvVar damlAssistantEnvVar DamlAssistantPath $
        pure (getDamlAssistantPathDefault damlPath damlVersion)

-- | Determine the absolute path to the assistant. Note that there is no
-- daml-head on Windows at the moment.
getDamlAssistantPathDefault :: DamlPath -> Maybe DamlAssistantSdkVersion -> DamlAssistantPath
getDamlAssistantPathDefault (DamlPath damlPath) damlVersion =
  let commandNameNoExt
          | Just (DamlAssistantSdkVersion v) <- damlVersion, isHeadVersion v = "daml-head"
          | otherwise = "daml"
      commandName
          | isWindows = commandNameNoExt <.> "cmd"
          | otherwise = commandNameNoExt
      path = damlPath </> "bin" </> commandName
  in DamlAssistantPath path

-- | Determine SDK version of running daml assistant. Can be overriden
-- with DAML_ASSISTANT_VERSION env var.
getDamlAssistantSdkVersion :: IO (Maybe DamlAssistantSdkVersion)
getDamlAssistantSdkVersion =
    overrideWithEnvVarMaybe damlAssistantVersionEnvVar
        (fmap DamlAssistantSdkVersion . parseVersion . pack)
        (fmap DamlAssistantSdkVersion <$> tryAssistantM getAssistantSdkVersion)

-- | Determine absolute path of daml home directory.
--
-- On Linux and Mac this is ~/.daml by default.
-- On Windows this is %APPDATA%/daml by default.
--
-- This default can be overriden with the DAML_HOME environment variable,
-- or by running from within an installed daml distribution, as
-- determined by the presence of a "daml-config.yaml" in the ascendants
-- of the executable path.
--
-- Raises an AssistantError if the path is missing.
getDamlPath :: IO DamlPath
getDamlPath = wrapErr "Determining daml home directory." $ do
    path <- required "Failed to determine daml path." =<< firstJustM id
        [ getEnv damlPathEnvVar
        , findM hasDamlConfig . ascendants =<< getExecutablePath
        , Just <$> getAppUserDataDirectory "daml"
        ]
    pure (DamlPath path)

    where
        hasDamlConfig :: FilePath -> IO Bool
        hasDamlConfig p = doesFileExist (p </> damlConfigName)

-- | Calculate the project path. This is done by starting at the current
-- working directory, checking if "daml.yaml" is present. If it is found,
-- that's the project path. Otherwise, go up one level and repeat
-- until you can't go up.
--
-- The project path can be overriden by passing the DAML_PROJECT
-- environment variable.
getProjectPath :: IO (Maybe ProjectPath)
getProjectPath = wrapErr "Detecting daml project." $ do
        pathM <- overrideWithEnvVarMaybe @SomeException projectPathEnvVar Right $ do
            cwd <- getCurrentDirectory
            findM hasProjectConfig (ascendants cwd)
        pure (ProjectPath <$> pathM)

    where
        hasProjectConfig :: FilePath -> IO Bool
        hasProjectConfig p = doesFileExist (p </> projectConfigName)

-- | Calculate the current SDK version and path.
--
-- These can be overriden by the environment variables DAML_SDK_VERSION
-- and DAML_SDK_PATH (and it ought to be enough to supply one of these
-- and have the other be inferred).
getSdk :: DamlPath
       -> Maybe DamlAssistantSdkVersion
       -> Maybe ProjectPath
       -> IO (Maybe SdkVersion, Maybe SdkPath)
getSdk damlPath damlAsstSdkVersionM projectPathM =
    wrapErr "Determining SDK version and path." $ do

        sdkVersion <- overrideWithEnvVarMaybe sdkVersionEnvVar (parseVersion . pack) $ firstJustM id
            [ maybeM (pure Nothing)
                (tryAssistantM . getSdkVersionFromSdkPath . SdkPath)
                (getEnv sdkPathEnvVar)
            , maybe (pure Nothing)
                (tryAssistantM . getSdkVersionFromProjectPath)
                projectPathM
            , tryAssistantM $ getDefaultSdkVersion damlPath
            ]

        sdkPath <- overrideWithEnvVarMaybe @SomeException sdkPathEnvVar (Right . SdkPath) $ firstJustM id
            [ useInstalledPath damlPath sdkVersion
            , autoInstall damlPath damlAsstSdkVersionM sdkVersion
            ]

        return (sdkVersion, sdkPath)

    where
        useInstalledPath :: DamlPath -> Maybe SdkVersion -> IO (Maybe SdkPath)
        useInstalledPath _ Nothing = pure Nothing
        useInstalledPath damlPath (Just sdkVersion) = do
            let sdkPath = defaultSdkPath damlPath sdkVersion
            test <- doesDirectoryExist (unwrapSdkPath sdkPath)
            pure (guard test >> Just sdkPath)

-- | Calculate the environment for dispatched commands (i.e. the environment
-- with updated DAML_HOME, DAML_PROJECT, DAML_SDK, etc).
getDispatchEnv :: Env -> IO [(String, String)]
getDispatchEnv Env{..} = do
    originalEnv <- getEnvironment
    pure $ filter ((`notElem` damlEnvVars) . fst) originalEnv
        ++ [ (damlPathEnvVar, unwrapDamlPath envDamlPath)
           , (projectPathEnvVar, maybe "" unwrapProjectPath envProjectPath)
           , (sdkPathEnvVar, maybe "" unwrapSdkPath envSdkPath)
           , (sdkVersionEnvVar, maybe "" versionToString envSdkVersion)
           , (sdkVersionLatestEnvVar, maybe "" versionToString envLatestStableSdkVersion)
           , (damlAssistantEnvVar, unwrapDamlAssistantPath envDamlAssistantPath)
           , (damlAssistantVersionEnvVar, maybe ""
               (versionToString . unwrapDamlAssistantSdkVersion)
               envDamlAssistantSdkVersion)
           ]

-- | Auto-installs requested version if it is missing and updates daml-assistant
-- if it is the latest stable version.
autoInstall
    :: DamlPath
    -> Maybe DamlAssistantSdkVersion
    -> Maybe SdkVersion
    -> IO (Maybe SdkPath)
autoInstall damlPath damlAsstSdkVersionM sdkVersionM = do
    damlConfigE <- tryConfig $ readDamlConfig damlPath
    let doAutoInstallE = queryDamlConfigRequired ["auto-install"] =<< damlConfigE
        doAutoInstall = fromRight True doAutoInstallE
    whenMaybe (doAutoInstall && isJust sdkVersionM) $ do
        let sdkVersion = fromJust sdkVersionM
        -- sdk is missing, so let's install it!
        let isLatest = maybe True ((< sdkVersion) . unwrapDamlAssistantSdkVersion)
                damlAsstSdkVersionM
            options = InstallOptions
                { iTargetM = Nothing
                , iQuiet = QuietInstall False
                , iActivate = ActivateInstall isLatest
                , iForce = ForceInstall False
                , iSetPath = SetPath True
                }
            env = InstallEnv
                { options = options
                , damlPath = damlPath
                , targetVersionM = Just sdkVersion
                , projectPathM = Nothing
                , output = hPutStrLn stderr
                    -- Print install messages to stderr since the install
                    -- is only happening because of some other command,
                    -- and we don't want to mess up the other command's
                    -- output / have the install messages be gobbled
                    -- up by a pipe.
                }
        versionInstall env sdkVersion
        pure (defaultSdkPath damlPath sdkVersion)

