{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Database.Znork where
import Dhall hiding (string,auto,map)
import Control.Lens
import qualified Dhall as D
import qualified Language.Haskell.TH.Syntax as TH
import Database.MSSql
import Database.Postgres
import Database.MySql
import Database.Sqlite
import Database.Oracle
import Database.Util

data DBSum a =
     MS (DBMS a)
   | PG (DBPG a)
   | MY (DBMY a)
   | OR (DBOracle a)
   | S3 (DBSqlite a)
   deriving (TH.Lift, Generic, Show, Eq)

makePrisms ''DBSum

instance FromDhall (DBSum a) where
  autoWith _ = toDBSum

instance ToDhall (DBSum a) where

-- union of a record and a single constructor
-- constructor is a functor only but record is applicative
toDBSum :: forall a. D.Type (DBSum a)
toDBSum = D.union (
     mempty
  <> constructor "MS" (MS <$> D.auto @(DBMS a))
  <> constructor "PG" (PG <$> D.auto @(DBPG a))
  <> constructor "MY" (MY <$> D.auto @(DBMY a))
  <> constructor "OR" (OR <$> D.auto @(DBOracle a))
  <> constructor "S3" (S3 <$> D.auto @(DBSqlite a))
  )

instance DConn (DBSum a) where
  connText = \case
                MS x -> connText x
                PG x -> connText x
                MY x -> connText x
                OR x -> connText x
                S3 x -> connText x
  getDbDefault _ = error "need to use 'a' not 'p a' for getDbDefault DBSum"


{-
>D.input D.auto "./test_db.dhall" :: IO (DBSum ())
MS (DBMS {_msdriver = "fred", _msserver = "asdf", _msauthn = Trusted, _msdb = "xx"})
it :: DBSum ()
-}

