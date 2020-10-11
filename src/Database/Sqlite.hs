-- Timeout=10000;NoTxn=1  [[default these: before they were dumped in with the driver
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
Module      : DBSqlite
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for ms sql server.
-}
module Database.Sqlite where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (ToText(..),st)
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Lens.TH (makeLenses)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Dhall
    ( (>*<),
      defaultInterpretOptions,
      encodeField,
      genericAutoWith,
      recordEncoder,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import Database.Util
import Data.Functor.Contravariant (Contravariant(contramap))
import Control.DeepSeq (NFData)

data DBSqlite a =
  DBSqlite
    { _s3driver :: !Text
    , _s3fn :: !Text
    , _s3dict :: !DbDict
    } deriving (TH.Lift, Show, Eq, Generic, Read)

makeLenses ''DBSqlite

instance NFData a => NFData (DBSqlite a)

instance FromDhall (DBSqlite a) where
  autoWith _i = dbsqlite

dbsqlite :: Decoder (DBSqlite a)
dbsqlite = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 3 }

instance ToDhall (DBSqlite a) where
  injectWith _o = recordEncoder $ contramap (\(DBSqlite a b c) -> (a, (b, c)))
         (encodeField @Text "driver" >*<
         encodeField @Text "fn" >*<
         encodeField @DbDict "dict")

instance ToText (DBSqlite a) where
  toText x = fromText $ _s3fn x

instance DConn (DBSqlite a) where
  connList DBSqlite {..} =
    [ ("Driver", wrapBraces _s3driver)
    , ("Database", _s3fn)
    ] <> unDict _s3dict
--  connText DBSqlite {..} = [st|#{_s3driver};Database=#{_s3fn};|] -- ;TraceFile=d:\haskell\s.log;|]
  getDbDefault _ = ''DBSqlite
  showDb DBSqlite {..} = [st|sqlite db=#{_s3fn}|]
  getSchema = const Nothing
  getDb = Just . _s3fn
  getDelims _ = Just ('"','"')
