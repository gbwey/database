-- todo: didnt use schema!
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TypeApplications #-}
{- |
Module      : DBPG
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for postgres.
-}
module Database.Postgres where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (ToText(..),st)
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Dhall
    ( Natural,
      (>*<),
      defaultInterpretOptions,
      encodeField,
      genericAutoWith,
      recordEncoder,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import Database.Util
import Data.Maybe (fromMaybe)
import Data.Functor.Contravariant (Contravariant(contramap))
import Control.DeepSeq (NFData)

data DBPG a =
  DBPG
    { pgdriver :: !Text
    , pgserver :: !Text
    , pgschema :: !(Maybe Text)
    , pguid :: !Text
    , pgpwd :: !Secret
    , pgdb :: !Text
    , pgport :: !(Maybe Natural)
    , pgdict :: !DbDict
    } deriving (TH.Lift, Show, Generic, Eq)

instance NFData a => NFData (DBPG a)

instance FromDhall (DBPG a) where
  autoWith _i = dbpostgres

dbpostgres :: Decoder (DBPG a)
dbpostgres = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "pg") }

instance ToDhall (DBPG a) where
  injectWith _o = recordEncoder $ contramap (\(DBPG a b c d e f g h) -> (a, (b, (c, (d, (e, (f, (g, h))))))))
         (encodeField @Text "driver" >*<
         encodeField @Text "server" >*<
         encodeField @(Maybe Text) "schema" >*<
         encodeField @Text "uid" >*<
         encodeField @Secret "pwd" >*<
         encodeField @Text "db" >*<
         encodeField @(Maybe Natural) "port" >*<
         encodeField @DbDict "dict")

instance ToText (DBPG a) where
  toText x = fromText $ maybe "" (<> ".") (pgschema x) <> pgdb x

instance DConn (DBPG a) where
  connList DBPG {..} =
    [ ("Driver", wrapBraces pgdriver)
    , ("Server", pgserver)
    , ("Uid", pguid)
    , ("Pwd", unSecret pgpwd)
    , ("Database", pgdb)
    , ("Port", T.pack (show (fromMaybe 5432 pgport)))
    ] <> maybe [] (\x -> [("Schema", x)]) pgschema
      <> unDict pgdict

-- connText DBPG {..} = [st|#{pgdriver};Server=#{pgserver};Port=#{maybe "5432" show pgport};Database=#{pgdb};Uid=#{pguid};Pwd=#{unSecret pgpwd};|]
  getDbDefault _ = ''DBPG
  showDb DBPG {..} = [st|postgres ip=#{pgserver} db=#{pgdb}|]
  getSchema = pgschema -- not sure how to specify the schema for postgres odbc
  getDb = Just . pgdb
  getDelims _ = Just ('\"','\"')
