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


data DBMY a = DBMY { _mydriver :: !Text
                   , _myserver :: !Text
                   , _myuid :: !Text
                   , _mypwd :: !Secret
                   , _mydb :: !Text
                   , _myport :: !(Maybe Natural)
                   } deriving (TH.Lift, Show, Generic, Read, Eq)

makeLenses ''DBMY

instance FromDhall (DBMY a) where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

instance ToDhall (DBMY a) where

instance ToText (DBMY a) where
  toText = fromText . _mydb

instance DConn (DBMY a) where
  connText DBMY {..} = [st|#{_mydriver};Server=#{_myserver};Port=#{maybe "3306" show _myport};Database=#{_mydb};User=#{_myuid};Password=#{unSecret _mypwd};option=67108864|]
  getDbDefault _ = ''DBMY
  showDb DBMY {..} = [st|mysql ip=#{_myserver} db=#{_mydb}|]
  getSchema = Just . _mydb -- no schemas within dbs ie treats dbs as if it is a schema!!!
  getDb = const Nothing
  getDelims _ = Just ('`','`')
