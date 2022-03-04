{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.Postgres
Description : Postgres Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for postgres.
-}
module Database.Postgres (
  DBPG (..),
  dbpostgres,
) where

import Control.DeepSeq (NFData)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (fromText)
import Database.Util
import Dhall (
  Decoder,
  FromDhall (..),
  Natural,
  ToDhall (..),
  encodeField,
  genericAutoWith,
  recordEncoder,
  (>*<),
 )
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Text.Shakespeare.Text (ToText (..), st)
import Prelude hiding (FilePath)

-- | postgres connection
data DBPG a = DBPG
  { pgdriver :: !Text
  , pgserver :: !Text
  , pgschema :: !(Maybe Text)
  , pguid :: !Text
  , pgpwd :: !Secret
  , pgdb :: !Text
  , pgport :: !(Maybe Natural)
  , pgdict :: !DbDict
  }
  deriving stock (TH.Lift, Show, Generic, Eq)

instance NFData a => NFData (DBPG a)

instance FromDhall (DBPG a) where
  autoWith _i = dbpostgres

-- | decoder for postgres
dbpostgres :: Decoder (DBPG a)
dbpostgres = genericAutoWith (fieldModDB "pg")

instance ToDhall (DBPG a) where
  injectWith _o =
    recordEncoder $
      contramap
        (\(DBPG a b c d e f g h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
        ( encodeField @Text "driver"
            >*< encodeField @Text "server"
            >*< encodeField @(Maybe Text) "schema"
            >*< encodeField @Text "uid"
            >*< encodeField @Secret "pwd"
            >*< encodeField @Text "db"
            >*< encodeField @(Maybe Natural) "port"
            >*< encodeField @DbDict "dict"
        )

instance ToText (DBPG a) where
  toText x = fromText $ maybe "" (<> ".") (pgschema x) <> pgdb x

instance DConn (DBPG a) where
  connList DBPG{..} =
    [ ("Driver", wrapOdbcBraces pgdriver)
    , ("Server", pgserver)
    , ("Uid", pguid)
    , ("Pwd", unSecret pgpwd)
    , ("Database", pgdb)
    , ("Port", T.pack (show (fromMaybe 5432 pgport)))
    ]
      <> maybe [] (\x -> [("Schema", x)]) pgschema
      <> unDict pgdict

  -- connText DBPG {..} = [st|#{pgdriver};Server=#{pgserver};Port=#{maybe "5432" show pgport};Database=#{pgdb};Uid=#{pguid};Pwd=#{unSecret pgpwd};|]
  getDbDefault _ = ''DBPG
  showDb DBPG{..} = [st|postgres ip=#{pgserver} db=#{pgdb}|]
  getSchema = pgschema -- not sure how to specify the schema for postgres odbc
  getDb = Just . pgdb
  getDelims _ = pure ('"', '"')
