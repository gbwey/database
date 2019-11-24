{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.DBSum
import Database.MSSql
import Test.Tasty
import Test.Tasty.HUnit
import qualified Dhall as D
import System.IO

ms :: IO (DBSum ())
ms = D.input D.auto "./test_db.dhall"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  defaultMain $ testGroup "Database"
    [
        testCase "ms" $ do
                          x <- ms
                          x @?= MS (DBMS "fred" "asdf" Trusted "xx")
    ]
