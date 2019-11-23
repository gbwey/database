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
Module      : DBOracle
Description : Oracle Database
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3
Maintainer  : gbwey9@gmail.com

Implementation of GConn for oracle.
-}
module Database.Oracle where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Lens.TH
import qualified Language.Haskell.TH.Syntax as TH
import Dhall hiding (maybe,string,map)
import qualified Dhall as D
import Database.Util

data OracleConnType = TnsName { _ocdriver :: !Text, _octns :: !Text } | DsnOracle !Text
  deriving (TH.Lift, Show, Generic, Read, Eq)

instance FromDhall OracleConnType where
  autoWith _ = toOCT

-- union of a record and a single constructor
-- constructor is a functor only but record is applicative
toOCT :: D.Type OracleConnType
toOCT = union
  (  constructor "TnsName" (record ( TnsName <$> field "driver" D.strictText <*> field "tns" D.strictText ))
  <> ( DsnOracle <$> constructor "DsnOracle" D.strictText)
  )

data DBOracle a = DBOracle { _orConnType :: OracleConnType
                           , _oruid :: !Text
                           , _orpwd :: !Secret
                           , _orschema :: !Text
                           } deriving (Eq, TH.Lift, Show, Generic, Read)

makeLenses ''DBOracle

instance FromDhall (DBOracle a) where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

instance ToText OracleConnType where
  toText = fromText . T.pack . show

instance ToDhall OracleConnType where

instance ToDhall (DBOracle a) where

instance ToText (DBOracle a) where
  toText = fromText . _orschema

instance DConn (DBOracle a) where
  connText DBOracle {..} =
    case _orConnType of
      TnsName driver tns -> [st|#{driver}; dbq=#{tns}; Uid=#{_oruid}; Pwd=#{unSecret _orpwd};|]
      DsnOracle dsn -> [st|DSN=#{dsn}; Uid=#{_oruid}; Pwd=#{unSecret _orpwd};|]
  getDbDefault _ = ''DBOracle
