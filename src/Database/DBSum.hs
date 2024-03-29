{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Database.DBSum
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3

Implementation of GConn for any database server
-}
module Database.DBSum (
  toDBSum,
  DBSum (..),
) where

import Control.DeepSeq (NFData)
import Data.Functor.Contravariant ((>$<))
import Database.MSSql (DBMS)
import Database.MySql (DBMY)
import Database.Oracle (DBOracle)
import Database.Postgres (DBPG)
import Database.Sqlite (DBSqlite)
import Database.Util (DConn (..))
import Dhall (
  FromDhall (..),
  ToDhall (..),
  constructor,
  encodeConstructorWith,
  inject,
  unionEncoder,
  (>|<),
 )
import qualified Dhall as D
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH

-- | sum all the databases into one type
data DBSum a
  = MS !(DBMS a)
  | PG !(DBPG a)
  | MY !(DBMY a)
  | OR !(DBOracle a)
  | S3 !(DBSqlite a)
  deriving stock (TH.Lift, Generic, Show, Eq)

instance NFData a => NFData (DBSum a)

instance FromDhall (DBSum a) where
  autoWith _ = toDBSum

instance ToDhall (DBSum a) where
  injectWith _ =
    adapt
      >$< unionEncoder
        ( encodeConstructorWith "MS" (inject @(DBMS a))
            >|< encodeConstructorWith "PG" (inject @(DBPG a))
            >|< encodeConstructorWith "MY" (inject @(DBMY a))
            >|< encodeConstructorWith "OR" (inject @(DBOracle a))
            >|< encodeConstructorWith "S3" (inject @(DBSqlite a))
        )
   where
    adapt ::
      DBSum a ->
      Either
        (DBMS a)
        ( Either
            (DBPG a)
            (Either (DBMY a) (Either (DBOracle a) (DBSqlite a)))
        )
    adapt (MS a) = Left a
    adapt (PG a) = Right (Left a)
    adapt (MY a) = Right (Right (Left a))
    adapt (OR a) = Right (Right (Right (Left a)))
    adapt (S3 a) = Right (Right (Right (Right a)))

-- union of a record and a single constructor
-- constructor is a functor only but record is applicative

-- | decoder for 'DBSum'
toDBSum :: forall a. D.Decoder (DBSum a)
toDBSum =
  D.union
    ( constructor "MS" (MS <$> D.auto @(DBMS a))
        <> constructor "PG" (PG <$> D.auto @(DBPG a))
        <> constructor "MY" (MY <$> D.auto @(DBMY a))
        <> constructor "OR" (OR <$> D.auto @(DBOracle a))
        <> constructor "S3" (S3 <$> D.auto @(DBSqlite a))
    )

instance DConn (DBSum a) where
  connList = \case
    MS x -> connList x
    PG x -> connList x
    MY x -> connList x
    OR x -> connList x
    S3 x -> connList x
  getDbDefault _ = error "need to use 'a' not 'p a' for getDbDefault DBSum"
  showDb = \case
    MS x -> showDb x
    PG x -> showDb x
    MY x -> showDb x
    OR x -> showDb x
    S3 x -> showDb x
  getSchema = \case
    MS x -> getSchema x
    PG x -> getSchema x
    MY x -> getSchema x
    OR x -> getSchema x
    S3 x -> getSchema x

  getDb = \case
    MS x -> getDb x
    PG x -> getDb x
    MY x -> getDb x
    OR x -> getDb x
    S3 x -> getDb x
  getDelims _ = error "need to use 'a' not 'p a' for getDelims DBSum"

{-
>D.input D.auto "./test_db.dhall" :: IO (DBSum ())
MS (DBMS {msdriver = "fred", msserver = "asdf", msauthn = Trusted, msdb = "xx"})
it :: DBSum ()
-}
