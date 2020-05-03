-- todo: didnt use schema!
{-# OPTIONS -Wall -Wcompat -Wincomplete-record-updates -Wincomplete-uni-patterns -Wredundant-constraints #-}
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
Maintainer  : gbwey9@gmail.com

Implementation of GConn for postgres.
-}
module Database.Postgres where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Lens.TH
import qualified Language.Haskell.TH.Syntax as TH
import Dhall hiding (maybe,string,map)
import Database.Util
import Data.Maybe
import Data.Functor.Contravariant

data DBPG a =
  DBPG
    { _pgdriver :: !Text
    , _pgserver :: !Text
    , _pgschema :: Maybe Text
    , _pguid :: !Text
    , _pgpwd :: !Secret
    , _pgdb :: !Text
    , _pgport :: !(Maybe Natural)
    , _pgdict :: !DbDict
    } deriving (TH.Lift, Show, Generic, Read, Eq)

makeLenses ''DBPG

instance FromDhall (DBPG a) where
  autoWith _i = dbpostgres

dbpostgres :: Decoder (DBPG a)
dbpostgres = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 3 }

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
  toText x = fromText $ maybe "" (<> ".") (_pgschema x) <> _pgdb x

instance DConn (DBPG a) where
  connList DBPG {..} =
    [ ("Driver", wrapBraces _pgdriver)
    , ("Server", _pgserver)
    , ("Uid", _pguid)
    , ("Pwd", unSecret _pgpwd)
    , ("Database", _pgdb)
    , ("Port", T.pack (show (fromMaybe 5432 _pgport)))
    ] <> maybe [] (\x -> [("Schema", x)]) _pgschema
      <> unDict _pgdict

-- connText DBPG {..} = [st|#{_pgdriver};Server=#{_pgserver};Port=#{maybe "5432" show _pgport};Database=#{_pgdb};Uid=#{_pguid};Pwd=#{unSecret _pgpwd};|]
  getDbDefault _ = ''DBPG
  showDb DBPG {..} = [st|postgres ip=#{_pgserver} db=#{_pgdb}|]
  getSchema = _pgschema -- not sure how to specify the schema for postgres odbc
  getDb = Just . _pgdb
  getDelims _ = Just ('\"','\"')
