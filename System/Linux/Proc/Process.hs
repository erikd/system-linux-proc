{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Process
  ( ProcessId (..)
  , getProcProcessIds
  ) where

import           Control.Error (runExceptT)

import           Data.Maybe (mapMaybe)

import           System.Linux.Proc.IO
import           System.Linux.Proc.Errors

newtype ProcessId
  = ProcessId { unProcessId :: Int }
  deriving (Eq, Show)

-- | Get the current list of `ProcessId`s.
getProcProcessIds :: IO (Either ProcError [ProcessId])
getProcProcessIds =
  runExceptT $
    mapMaybe maybeProcessId <$> listProcDirectory "/proc"
  where
    maybeProcessId :: String -> Maybe ProcessId
    maybeProcessId str =
      case reads str of
        [(i, "")] -> Just $ ProcessId i
        _ -> Nothing
