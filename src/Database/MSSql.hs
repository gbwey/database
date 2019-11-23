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
module Database.MSSql where
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

data MSAuthn = Trusted | UserPwd { _msUser :: Text, _msPassword :: Secret }
  deriving (TH.Lift, Show, Eq, Generic, Read)

makePrisms ''MSAuthn

instance FromDhall MSAuthn where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

data DBMS a = DBMS {
                 _msdriver :: !Text
               , _msserver :: !Text
               , _msauthn :: !MSAuthn
               , _msdb :: !Text
               } deriving (TH.Lift, Show, Eq, Generic, Read)

makeLenses ''DBMS

instance FromDhall (DBMS a) where
  autoWith i = genericAutoY i { fieldModifier = T.drop 3 }

instance ToDhall MSAuthn where

instance ToDhall (DBMS a) where

instance ToText (DBMS a) where
  toText = fromText . _msdb

instance DConn (DBMS a) where
  connText DBMS {..} = [st|#{_msdriver};Server=#{_msserver};Database=#{_msdb};#{connAuth _msauthn};|]
  getDbDefault _ = ''DBMS

connAuth :: MSAuthn -> String
connAuth Trusted = "Trusted_Connection=yes"
connAuth (UserPwd uid (Secret pwd)) = T.unpack [st|uid=#{uid};pwd=#{pwd}|]

