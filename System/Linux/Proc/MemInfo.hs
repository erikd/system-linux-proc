{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.MemInfo
  ( MemInfo (..)
  , readProcMemInfo
  , readProcMemUsage
  ) where

import           Control.Error (fromMaybe, runExceptT, throwE)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as DL
import qualified Data.Map.Strict as DM
import qualified Data.Text as T
import           Data.Word (Word64)

import           System.Linux.Proc.File
import           System.Linux.Proc.Errors

-- | A struct to contain information parsed from the `/proc/meminfo` file
-- (Linux only AFAIK). Fields that are listed as being in kilobytes in the proc
-- filesystem are converted to bytes.
-- Not all versions of the Linux kernel make all the fields in this struct
-- available in which case they will be assigned a value of zero.
data MemInfo = MemInfo
  { memTotal :: !Word64 -- ^ Total physical RAM.
  , memFree :: !Word64 -- ^ Total free RAM (which includes memory used for filesystem caching).
  , memAvailable :: !Word64 -- ^ Available memory.
  , memBuffers :: !Word64 -- ^ Amount of RAM used for file buffers.
  , memSwapTotal :: !Word64 -- ^ Total about of swap space.
  , memSwapFree :: !Word64 -- ^ Amount of swap space that is free.
  } deriving (Eq, Show)

-- | Read the `/proc/meminfo` file (Linux only AFAIK) and return a
-- `MemInfo` structure.
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMemInfo :: IO (Either ProcError MemInfo)
readProcMemInfo =
  runExceptT $ do
    bs <- readProcFile fpMemInfo
    case A.parseOnly parseFields bs of
      Left e -> throwE $ ProcParseError fpMemInfo (T.pack e)
      Right xs -> pure $ construct xs

-- | Read `/proc/meminfo` file and return a value calculated from:
--
--      MemAvailable / MemTotal
--
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMemUsage :: IO (Either ProcError Double)
readProcMemUsage =
  runExceptT $ do
    xs <- BS.lines <$> readProcFile fpMemInfo
    pure . convert $ DL.foldl' getValues (0, 1) xs
  where
    getValues :: (Word64, Word64) -> ByteString -> (Word64, Word64)
    getValues (avail, total) bs =
      case BS.break (== ':') bs of
        ("MemTotal", rest) -> (avail, fromEither total $ A.parseOnly pValue rest)
        ("MemAvailable", rest) -> (fromEither avail $ A.parseOnly pValue rest, total)
        _ -> (avail, total)

    convert :: (Word64, Word64) -> Double
    convert (avail, total) = fromIntegral avail / fromIntegral total

    fromEither :: a -> Either e a -> a
    fromEither a (Left _) = a
    fromEither _ (Right a) = a

-- -----------------------------------------------------------------------------
-- Internals.

fpMemInfo :: FilePath
fpMemInfo = "/proc/meminfo"

construct :: [(ByteString, Word64)] -> MemInfo
construct xs =
  MemInfo
    (fromMaybe 0 $ DM.lookup "MemTotal" mp)
    (fromMaybe 0 $ DM.lookup "MemFree" mp)
    (fromMaybe 0 $ DM.lookup "MemAvailable" mp)
    (fromMaybe 0 $ DM.lookup "Buffers" mp)
    (fromMaybe 0 $ DM.lookup "SwapTotal" mp)
    (fromMaybe 0 $ DM.lookup "SwapFree" mp)
  where
    mp = DM.fromList xs

-- -----------------------------------------------------------------------------
-- Parsers.

parseFields :: Parser [(ByteString, Word64)]
parseFields =
  A.many1 (pFieldValue <* A.endOfLine)


{-
The /proc/meminfo file's contents takes the form:

    MemTotal:       16336908 kB
    MemFree:         9605680 kB
    MemAvailable:   12756896 kB
    Buffers:         1315348 kB
    ....

-}

pFieldValue :: Parser (ByteString, Word64)
pFieldValue =
  (,) <$> pName <*> pValue


pName :: Parser ByteString
pName =
  A.takeWhile (/= ':')

pValue :: Parser Word64
pValue = do
  val <- A.char ':' *> A.skipSpace *> A.decimal
  A.skipSpace
  rest <- A.takeWhile (not . A.isSpace)
  case rest of
    "" -> pure val
    "kB" -> pure $ 1024 * val
    _ -> fail $ "Unexpected '" ++ BS.unpack rest ++ "'"

