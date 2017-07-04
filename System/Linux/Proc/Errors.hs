{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Errors
  ( ProcError (..)
  , renderProcError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T


data ProcError
  = ProcReadError !FilePath !Text
  | ProcParseError !FilePath !Text
  | ProcMemInfoKeyError !Text
  deriving (Eq, Show)

renderProcError :: ProcError -> Text
renderProcError = \case
  ProcReadError fp msg -> T.concat
    [ "Error reading '", T.pack fp, "': ", msg ]
  ProcParseError fp msg -> T.concat
    [ "Parser error on file '", T.pack fp, ": ", msg ]
  ProcMemInfoKeyError key -> T.concat
    [ "MemInfo: Key not found: '", key, "'" ]


