{-# LANGUAGE TemplateHaskell #-}
module Test.System.Linux.Proc
  ( tests
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Hedgehog (Property, discover)
import qualified Hedgehog as H
-- import qualified Hedgehog.Gen as Gen

import           System.Linux.Proc

import           Test.System.Linux.Proc.Hedgehog

prop_read_MemInfo :: Property
prop_read_MemInfo =
  propertyWithTests 10 $ do
    mi <- H.evalEither =<< liftIO readProcMemInfo
    assertShow mi (memTotal mi > memAvailable mi)
    assertShow mi (memTotal mi > memFree mi)
    assertShow mi (memTotal mi > memBuffers mi)
    assertShow mi (memSwapTotal mi >= memSwapFree mi)

prop_read_ProcessIds :: Property
prop_read_ProcessIds =
  propertyWithTests 10 $ do
    pids <- H.evalEither =<< liftIO getProcProcessIds
    assertShow pids (length pids > 0)

-- -----------------------------------------------------------------------------

tests :: IO Bool
tests =
  H.checkParallel $$discover
