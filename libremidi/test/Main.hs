{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import Libremidi.Api qualified as LMA
import Libremidi.Simple qualified as LMS
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

testMetadata :: TestTree
testMetadata = testCase "metadata" $ do
  ver <- LMA.getVersion
  ver @?= "4.5.0"
  avail1 <- LMS.listAvailApis1
  assertBool "midi1 supports dummy" (LMA.ApiDummy `elem` avail1)
  avail2 <- LMS.listAvailApis2
  assertBool "midi2 supports dummy" (LMA.ApiDummy `elem` avail2)
  dummyIdent <- LMA.apiIdentifier LMA.ApiDummy
  dummyIdent @?= "dummy"
  dummyName <- LMA.apiDisplayName LMA.ApiDummy
  dummyName @?= "Dummy"
  dummyApi <- LMA.getCompiledApiByIdentifier "dummy"
  dummyApi @?= Just LMA.ApiDummy

main :: IO ()
main =
  defaultMain $
    testGroup
      "Libremidi"
      [ testMetadata
      ]
