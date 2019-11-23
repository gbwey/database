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
Module      : DBSqlite
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for ms sql server.
-}
module Database.Sqlite where
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
import System.IO

data DBSqlite a = DBSqlite { _s3driver :: !Text
                           , _s3schema :: !(Maybe Text)
                           , _s3fn :: !FilePath
                           } deriving (TH.Lift, Show, Eq, Generic, Read)

makeLenses ''DBSqlite

instance FromDhall (DBSqlite a) where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

instance ToDhall (DBSqlite a) where

instance ToText (DBSqlite a) where
  toText x = fromText $ maybe "" (<> ".") (_s3schema x) <> T.pack (_s3fn x)

instance DConn (DBSqlite a) where
  connText DBSqlite {..} = [st|#{_s3driver};Database=#{_s3fn};|] -- ;TraceFile=d:\haskell\s.log;|]
  getDbDefault _ = ''DBSqlite
