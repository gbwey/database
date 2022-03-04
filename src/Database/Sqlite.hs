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
-- Timeout=10000;NoTxn=1  [[default these: before they were dumped in with the driver
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.Sqlite
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for sqlite.
-}
module Database.Sqlite (
  DBSqlite (..),
  dbsqlite,
) where

import Control.DeepSeq (NFData)
import Data.Functor.Contravariant (Contravariant (contramap))
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText)
import Database.Util
import Dhall (
  Decoder,
  FromDhall (..),
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

-- | sqlite connection
data DBSqlite a = DBSqlite
  { s3driver :: !Text
  , s3fn :: !Text
  , s3dict :: !DbDict
  }
  deriving stock (TH.Lift, Show, Eq, Generic)

instance NFData a => NFData (DBSqlite a)

instance FromDhall (DBSqlite a) where
  autoWith _i = dbsqlite

-- | decoder for sqlite
dbsqlite :: Decoder (DBSqlite a)
dbsqlite = genericAutoWith (fieldModDB "s3")

instance ToDhall (DBSqlite a) where
  injectWith _o =
    recordEncoder $
      contramap
        (\(DBSqlite a b c) -> (a, (b, c)))
        ( encodeField @Text "driver"
            >*< encodeField @Text "fn"
            >*< encodeField @DbDict "dict"
        )

instance ToText (DBSqlite a) where
  toText x = fromText $ s3fn x

instance DConn (DBSqlite a) where
  connList DBSqlite{..} =
    [ ("Driver", wrapOdbcBraces s3driver)
    , ("Database", s3fn)
    ]
      <> unDict s3dict

  --  connText DBSqlite {..} = [st|#{s3driver};Database=#{s3fn};|] -- ;TraceFile=d:\haskell\s.log;|]
  getDbDefault _ = ''DBSqlite
  showDb DBSqlite{..} = [st|sqlite db=#{s3fn}|]
  getSchema = const Nothing
  getDb = Just . s3fn
  getDelims _ = pure ('"', '"')
