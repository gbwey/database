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
-- need to add this to dictionary in dhall option=67108864
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.MySql
Description : MySql Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for mysql.
-}
module Database.MySql (
  DBMY (..),
  dbmysql,
) where

import Control.DeepSeq (NFData)
import Data.Functor.Contravariant
import Data.List.NonEmpty (NonEmpty (..))
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

-- | mysql connection settings
data DBMY a = DBMY
  { mydriver :: !Text
  , myserver :: !Text
  , myuid :: !Text
  , mypwd :: !Secret
  , mydb :: !Text
  , myport :: !(Maybe Natural)
  , mydict :: !DbDict
  }
  deriving stock (TH.Lift, Show, Generic, Eq)

instance NFData a => NFData (DBMY a)

instance FromDhall (DBMY a) where
  autoWith _i = dbmysql

-- | decoder for mysql
dbmysql :: Decoder (DBMY a)
dbmysql = genericAutoWith (fieldModDB "my")

instance ToDhall (DBMY a) where
  injectWith _o =
    recordEncoder $
      contramap
        (\(DBMY a b c d e f g) -> (a, (b, (c, (d, (e, (f, g)))))))
        ( encodeField @Text "driver"
            >*< encodeField @Text "server"
            >*< encodeField @Text "uid"
            >*< encodeField @Secret "pwd"
            >*< encodeField @Text "db"
            >*< encodeField @(Maybe Natural) "port"
            >*< encodeField @DbDict "dict"
        )

instance ToText (DBMY a) where
  toText = fromText . mydb

-- {mydriver};Server=#{myserver};Port=#{maybe "3306" show myport};Database=#{mydb};User=#{myuid};Password=#{unSecret mypwd};option=67108864;#{extra}|]

instance DConn (DBMY a) where
  connList DBMY{..} =
    [ ("Driver", wrapOdbcBraces mydriver)
    , ("Server", myserver)
    , ("Port", T.pack (show (fromMaybe 3306 myport)))
    , ("Database", mydb)
    , ("User", myuid)
    , ("Password", unSecret mypwd)
    ]
      <> unDict mydict
  getDbDefault _ = ''DBMY
  showDb DBMY{..} = [st|mysql ip=#{myserver} db=#{mydb}|]
  getSchema = Just . mydb -- no schemas within dbs ie treats dbs as if it is a schema!!!
  getDb = const Nothing
  getDelims _ = ('`', '`') :| [('"', '"')]
