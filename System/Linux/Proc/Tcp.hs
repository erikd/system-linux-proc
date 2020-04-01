{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.Tcp
  ( TcpSocket(..)
  , TcpState(..)
  , readProcTcpSockets
  )
where

import           System.Linux.Proc.Errors       ( ProcError(..) )
import           System.Linux.Proc.Process      ( ProcessId(..) )
import           System.Linux.Proc.IO           ( readProcFile )

import           Control.Error                  ( runExceptT
                                                , throwE
                                                )

import           Data.Attoparsec.ByteString.Char8
                                                ( Parser )
import qualified Data.Attoparsec.ByteString.Char8
                                               as A
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text                     as T
import           Data.List                      ( intersperse )
import           Data.Bits                      ( shiftL
                                                , (.|.)
                                                )

import           Control.Monad                  ( replicateM )


data TcpState =
    TcpEstablished
  | TcpSynSent
  | TcpSynReceive
  | TcpFinWait1
  | TcpFinWait2
  | TcpTimeWait
  | TcpClose
  | TcpCloseWait
  | TcpLastAck
  | TcpListen
  | TcpClosing
  | TcpNewSynReceive
  deriving (Show, Eq)

-- | TCP socket used by a process according to the `/proc/<pid>/net/tcp`
-- file of the process. Only non-debug fields are parsed and described the socket
-- data structure.
data TcpSocket = TcpSocket {
    tcpLocalAddr :: !(BS.ByteString, Int),
    tcpRemoteAddr :: !(BS.ByteString, Int),
    tcpState :: !TcpState,
    tcpUid :: !Int,
    tcpInode :: !Int
} deriving (Show)


-- | Read and parse the `/proc/<pid>/net/tcp` file. Read and parse errors are caught
-- and returned.
readProcTcpSockets :: ProcessId -> IO (Either ProcError [TcpSocket])
readProcTcpSockets pid = runExceptT $ do
  let fpNetTcp' = fpNetTcp pid
  bs <- readProcFile fpNetTcp'
  case A.parseOnly (parseTcpSockets <* A.endOfInput) bs of
    Left  e  -> throwE $ ProcParseError fpNetTcp' (T.pack e)
    Right ss -> pure ss


-- -----------------------------------------------------------------------------
-- Internals.

fpNetTcp :: ProcessId -> FilePath
fpNetTcp (ProcessId pid) = "/proc/" ++ show pid ++ "/net/tcp"


-- -----------------------------------------------------------------------------
-- Parsers.

parseTcpSockets :: Parser [TcpSocket]
parseTcpSockets = headers *> A.many' record

-- Parse a single space. The net/tcp file does not use tabs. Attoparsec's space
-- includes tab, newline and return feed which captures too much in our case.
space :: Parser Char
space = A.char ' '

headers :: Parser BS.ByteString
headers =
  A.many1 space
    *> A.string
         "sl  local_address rem_address   st tx_queue rx_queue tr tm->when retrnsmt   uid  timeout inode"
    <* A.many1 space
    <* A.endOfLine

record :: Parser TcpSocket
record = do
  _          <- delim
  _          <- (A.many1 A.digit *> A.char ':') <* delim -- Parse kernel slot
  localAddr  <- addr <* delim
  remoteAddr <- addr <* delim
  tcpState'  <- state <* delim
  _          <- internalData
  uid        <- A.decimal <* delim :: Parser Int
  _          <- A.hexadecimal <* delim :: Parser Int -- internal kernel state
  inode      <- A.decimal <* delim :: Parser Int
  _          <- A.many1 (A.satisfy (/= '\n')) -- remaining internal state
  _          <- A.endOfLine
  return $ TcpSocket localAddr remoteAddr tcpState' uid inode
  where delim = A.many1 space

internalData :: Parser ()
internalData = do
  _ <- A.hexadecimal :: Parser Int -- outgoing data queue
  _ <- A.char ':'
  _ <- A.hexadecimal :: Parser Int -- incoming data queue
  _ <- A.many1 space
  _ <- A.hexadecimal :: Parser Int -- internal kernel state
  _ <- A.char ':'
  _ <- A.hexadecimal :: Parser Int -- internal kernel state
  _ <- A.many1 space
  _ <- A.hexadecimal :: Parser Int -- internal kernel state
  _ <- A.many1 space
  return ()

-- The address parts of the `net/tcp` file is a hexadecimal representation of the IP
-- address and the port. The octets of the IP address have been reversed: 127.0.0.1
-- has been reversed to 1.0.0.127 and then rendered as hex numbers. The port is only
-- rendered as a hex number; it's not been reversed.
addr :: Parser (BS.ByteString, Int)
addr = do
  addrParts <- replicateM 4 $ hexadecimalOfLength 2
  _         <- A.char ':'
  port      <- hexadecimalOfLength 4
  let addr' =
        BS.concat . intersperse "." . fmap (BS.pack . show) $ reverse addrParts
  return (addr', port)

-- See include/net/tcp_states.h of your kernel's source code for all possible states.
state :: Parser TcpState
state = lookupState <$> (A.char '0' *> A.satisfy (A.inClass "1-9A-C"))
 where
  lookupState :: Char -> TcpState
  lookupState '1' = TcpEstablished
  lookupState '2' = TcpSynSent
  lookupState '3' = TcpSynReceive
  lookupState '4' = TcpFinWait1
  lookupState '5' = TcpFinWait2
  lookupState '6' = TcpTimeWait
  lookupState '7' = TcpClose
  lookupState '8' = TcpCloseWait
  lookupState '9' = TcpLastAck
  lookupState 'A' = TcpListen
  lookupState 'B' = TcpClosing
  lookupState 'C' = TcpNewSynReceive
  lookupState _   = undefined -- Intentionally undefined.

-- Helper parser for hexadecimal strings of a known length. Attoparsec's hexadecimal
-- will keep parsing digits to cover cases like '1', 'AB2', 'deadbeef', etc. In our
-- case we need to parse cases of exact length like port numbers.
hexadecimalOfLength :: Int -> Parser Int
hexadecimalOfLength n = do
  ds <- A.count n (A.satisfy (isHexDigit . fromEnum))
  return $ foldl step 0 (fmap (fromEnum :: Char -> Int) ds)
 where
  isHexDigit :: Int -> Bool
  isHexDigit w =
    (w >= 48 && w <= 57) || (w >= 97 && w <= 102) || (w >= 65 && w <= 70)
  step :: Int -> Int -> Int
  step a w | w >= 48 && w <= 57 = (a `shiftL` 4) .|. (w - 48)
           | w >= 97            = (a `shiftL` 4) .|. (w - 87)
           | otherwise          = (a `shiftL` 4) .|. (w - 55)
