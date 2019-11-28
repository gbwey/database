{-# OPTIONS -Wall #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Database.DBSum
import Database.MSSql
import Database.Oracle
import Database.MySql
import Database.Postgres
import Database.Sqlite
import Database.Util
import Test.Tasty
import Test.Tasty.HUnit
import qualified Dhall as D
import System.IO

msCfg, mstrustedCfg, orCfg, pgCfg, myCfg, s3Cfg :: IO (DBSum ())
msCfg = D.input D.auto "./test_dbms.dhall"
mstrustedCfg = D.input D.auto "./test_dbms_trusted.dhall"
orCfg= D.input D.auto "./test_dbor.dhall"
pgCfg = D.input D.auto "./test_dbpg.dhall"
myCfg = D.input D.auto "./test_dbmy.dhall"
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
  defaultMain $ testGroup "Database"
    [
      testCase "ms" $ ms @?= MS (DBMS "{msdriver}" "msserver" (UserPwd "msuid" "mspwd") "msdb" (DbDict [("ms1","aa"), ("ms2","bb")]))
    , testCase "ms trusted" $ mst @?= MS (DBMS "{msdrivert}" "msservert"  Trusted "msdbt" (DbDict [("ms1t","aat"), ("ms2t","bbt")]))
    , testCase "oracle" $ orx @?= OR (DBOracle (TnsName "{ordriver}" "ordbq") "oruid" "orpwd" "orschema" (DbDict [("or1","aa"), ("or2","bb")]))
    , testCase "postgres" $ pg @?= PG (DBPG "{pgdriver}" "pgserver" (Just "pgschema") "pguid" "pgpwd" "pgdb" Nothing (DbDict [("pg1","aa"), ("pg2","bb")]))
    , testCase "sqlite" $ s3 @?= S3 (DBSqlite "{s3driver}" "s3db" (DbDict [("s31","aa"), ("s32","bb")]))
    , testCase "mysql" $ my @?= MY (DBMY "{mydriver}" "myserver" "myuid" "mypwd" "mydb" Nothing (DbDict [("my1","aa"), ("my2","bb")]))
    ]
