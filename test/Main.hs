-- staticDhallExpression checks for syntax only! doesnt load into haskell type
{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Database.DBSum
import Database.MSSql
import Test.Tasty
import Test.Tasty.HUnit
import qualified Dhall as D

ms :: IO (DBSum ())
ms = D.input D.auto "./test_db.dhall"

main :: IO ()
main = defaultMain $ testGroup "Database"
    [
        testCase "ms" $ do
                          x <- ms
                          x @?= MS (DBMS "fred" "asdf" Trusted "xx")
    ]
