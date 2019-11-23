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

data DBPG a = DBPG {
                     _pgdriver :: !Text
                   , _pgserver :: !Text
                   , _pgschema :: Maybe Text
                   , _pguid :: !Text
                   , _pgpwd :: !Secret
                   , _pgdb :: !Text
                   , _pgport :: !(Maybe Natural)
                   } deriving (TH.Lift, Show, Generic, Read, Eq)

makeLenses ''DBPG

instance FromDhall (DBPG a) where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

instance ToDhall (DBPG a) where

instance ToText (DBPG a) where
  toText x = fromText $ maybe "" (<> ".") (_pgschema x) <> _pgdb x

instance DConn (DBPG a) where
  connText DBPG {..} = [st|#{_pgdriver};Server=#{_pgserver};Port=#{maybe "5432" show _pgport};Database=#{_pgdb};Uid=#{_pguid};Pwd=#{unSecret _pgpwd};|]
  getDbDefault _ = ''DBPG
