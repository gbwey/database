{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Database.DBSum
import Database.MSSql
import Database.MySql
import Database.Oracle
import Database.Postgres
import Database.Sqlite
import Database.Util
import Dhall hiding (maybe)
import qualified Dhall as D
import System.IO
import Test.Tasty
import Test.Tasty.HUnit
import Text.Shakespeare.Text

msCfgSum, orCfgSum, pgCfgSum, myCfgSum, s3CfgSum :: IO (DBSum ())
msCfgSum = D.input D.auto "./test_dbms_sum.dhall"
orCfgSum = D.input D.auto "./test_dbor_sum.dhall"
pgCfgSum = D.input D.auto "./test_dbpg_sum.dhall"
myCfgSum = D.input D.auto "./test_dbmy_sum.dhall"
s3CfgSum = D.input D.auto "./test_dbs3_sum.dhall"

msCfg, mstrustedCfg :: IO (DBMS ())
msCfg = D.input D.auto "./test_dbms.dhall"
mstrustedCfg = D.input D.auto "./test_dbms_trusted.dhall"

orCfg :: IO (DBOracle ())
orCfg = D.input D.auto "./test_dbor.dhall"

pgCfg :: IO (DBPG ())
pgCfg = D.input D.auto "./test_dbpg.dhall"

myCfg :: IO (DBMY ())
myCfg = D.input D.auto "./test_dbmy.dhall"

s3Cfg :: IO (DBSqlite ())
s3Cfg = D.input D.auto "./test_dbs3.dhall"

main :: IO ()
main = do
  hSetEncoding stdout utf8
  ms <- msCfg
  mst <- mstrustedCfg
  orx <- orCfg
  pg <- pgCfg
  my <- myCfg
  s3 <- s3Cfg

  mssum <- msCfgSum
  orsum <- orCfgSum
  pgsum <- pgCfgSum
  mysum <- myCfgSum
  s3sum <- s3CfgSum
  defaultMain $
    testGroup
      "Database"
      [ testCase "sum ms" $ mssum @?= MS (DBMS "{msdriver}" "msserver" (UserPwd "msuid" "mspwd") "msdb" (DbDict [("ms1", "aa"), ("ms2", "bb")]))
      , testCase "sum oracle" $ orsum @?= OR (DBOracle (TnsName "{ordriver}" "ordbq") "oruid" "orpwd" "orschema" (DbDict [("or1", "aa"), ("or2", "bb")]))
      , testCase "sum postgres" $ pgsum @?= PG (DBPG "{pgdriver}" "pgserver" (Just "pgschema") "pguid" "pgpwd" "pgdb" Nothing (DbDict [("pg1", "aa"), ("pg2", "bb")]))
      , testCase "sum sqlite" $ s3sum @?= S3 (DBSqlite "{s3driver}" "s3db" (DbDict [("s31", "aa"), ("s32", "bb")]))
      , testCase "sum mysql" $ mysum @?= MY (DBMY "{mydriver}" "myserver" "myuid" "mypwd" "mydb" Nothing (DbDict [("my1", "aa"), ("my2", "bb")]))
      , testCase "ms" $ ms @?= DBMS "{msdriver}" "msserver" (UserPwd "msuid" "mspwd") "msdb" (DbDict [("ms1", "aa"), ("ms2", "bb")])
      , testCase "ms trusted" $ mst @?= DBMS "{msdrivert}" "msservert" Trusted "msdbt" (DbDict [("ms1t", "aat"), ("ms2t", "bbt")])
      , testCase "oracle" $ orx @?= DBOracle (TnsName "{ordriver}" "ordbq") "oruid" "orpwd" "orschema" (DbDict [("or1", "aa"), ("or2", "bb")])
      , testCase "postgres" $ pg @?= DBPG "{pgdriver}" "pgserver" (Just "pgschema") "pguid" "pgpwd" "pgdb" Nothing (DbDict [("pg1", "aa"), ("pg2", "bb")])
      , testCase "sqlite" $ s3 @?= DBSqlite "{s3driver}" "s3db" (DbDict [("s31", "aa"), ("s32", "bb")])
      , testCase "mysql" $ my @?= DBMY "{mydriver}" "myserver" "myuid" "mypwd" "mydb" Nothing (DbDict [("my1", "aa"), ("my2", "bb")])
      , testCase "todhall: ms trusted" $ testmstodhall >>= \f -> f (DBMS "driver2" "server2" Trusted "db2" (DbDict [("x2", "y2")])) @?= Nothing
      , testCase "todhall: ms authn" $ testmstodhall >>= \f -> f (DBMS "driver2" "server2" (UserPwd "user2" "pwd2") "db2" (DbDict [("x2", "y2")])) @?= Just ("user2", "pwd2")
      , testCase "todhall: or dsn" $ testortodhall >>= \f -> f (DBOracle (DsnOracle "dsn1") "schema1" "user1" "pwd1" (DbDict [("x1", "y1")])) @?= ("dsn1", Nothing)
      , testCase "todhall: or driver" $ testortodhall >>= \f -> f (DBOracle (TnsName "driver1" "tns1") "schema1" "user1" "pwd1" (DbDict [("x1", "y1")])) @?= ("driver1", Just "tns1")
      ]

testmstodhall :: IO (DBMS a -> Maybe (Text, Text))
testmstodhall =
  let txt =
        [st|
    let x = ./corems.dhall
    let t = { _1 : Text, _2 : Text }
    in \(y : x.Type) ->
       merge { Trusted = None t
             , UserPwd = \(z : { user : Text, password : Text }) -> Some { _1 = z.user, _2 = z.password }
             } y.authn : Optional t
|]
   in input auto txt

testortodhall :: IO (DBOracle a -> (Text, Maybe Text))
testortodhall =
  let txt =
        [st|
    let x = ./coreor.dhall
    let t = { _1 : Text, _2 : Optional Text }
    in \(y : x.Type) ->
       merge { DsnOracle = \(z : Text) -> { _1 = z, _2 = None Text }
             , TnsName = \(z : { driver : Text, tns : Text }) -> { _1 = z.driver, _2 = Some z.tns }
             } y.ConnType : t
|]
   in input auto txt
