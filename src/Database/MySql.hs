-- need to add this to dictionary in dhall option=67108864
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
Module      : DBMY
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for mysql.
-}
module Database.MySql where
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

data DBMY a =
  DBMY
    { _mydriver :: !Text
    , _myserver :: !Text
    , _myuid :: !Text
    , _mypwd :: !Secret
    , _mydb :: !Text
    , _myport :: !(Maybe Natural)
    , _mydict :: !DbDict
    } deriving (TH.Lift, Show, Generic, Read, Eq)

makeLenses ''DBMY

instance FromDhall (DBMY a) where
  autoWith _i = dbmysql

dbmysql :: Decoder (DBMY a)
dbmysql = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 3 }

instance ToDhall (DBMY a) where
  injectWith _o = recordEncoder $ (\x -> contramap (\(DBMY a b c d e f g) -> (a, (b, (c, (d, (e, (f, g))))))) x)
         ((encodeField @Text "driver") >*<
         (encodeField @Text "server") >*<
         (encodeField @Text "uid") >*<
         (encodeField @Secret "pwd") >*<
         (encodeField @Text "db") >*<
         (encodeField @(Maybe Natural) "port") >*<
         (encodeField @DbDict "dict"))


instance ToText (DBMY a) where
  toText = fromText . _mydb

-- {_mydriver};Server=#{_myserver};Port=#{maybe "3306" show _myport};Database=#{_mydb};User=#{_myuid};Password=#{unSecret _mypwd};option=67108864;#{extra}|]

instance DConn (DBMY a) where
  connList DBMY {..} =
    [ ("Driver", wrapBraces _mydriver)
    , ("Server", _myserver)
    , ("Port", T.pack (show (fromMaybe 3306 _myport)))
    , ("Database", _mydb)
    , ("User", _myuid)
    , ("Password", unSecret _mypwd)
    ] <> unDict _mydict
  getDbDefault _ = ''DBMY
  showDb DBMY {..} = [st|mysql ip=#{_myserver} db=#{_mydb}|]
  getSchema = Just . _mydb -- no schemas within dbs ie treats dbs as if it is a schema!!!
  getDb = const Nothing
  getDelims _ = Just ('`','`')
