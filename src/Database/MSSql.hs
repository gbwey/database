{-# OPTIONS -Wno-partial-fields #-}
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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.MSSql
Description : MSSQL Server
Copyright   : (c) Grant Weyburne, 2016
License     : BSD-3

Implementation of GConn for mssql.
-}
module Database.MSSql (
  MSAuthn (..),
  DBMS (..),
  dbmssql,
  connAuth,
) where

import Control.DeepSeq (NFData)
import Data.Functor.Contravariant (Contravariant (contramap), (>$<))
import Data.Functor.Contravariant.Divisible
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Lazy.Builder (fromText)
import Database.Util
import Dhall (
  Decoder,
  FromDhall (..),
  ToDhall (..),
  encodeConstructorWith,
  encodeField,
  genericAutoWith,
  inject,
  recordEncoder,
  unionEncoder,
  (>*<),
  (>|<),
 )
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Text.Shakespeare.Text (ToText (..), st)
import Prelude hiding (FilePath)

-- | authentication options for mssql
data MSAuthn = Trusted | UserPwd {msuser :: !Text, mspassword :: !Secret}
  deriving stock (TH.Lift, Show, Eq, Generic)

instance NFData MSAuthn

instance FromDhall MSAuthn where
  autoWith _i = genericAutoWith (fieldModDB "ms")

-- | mssql connection settings
data DBMS a = DBMS
  { msdriver :: !Text
  , msserver :: !Text
  , msauthn :: !MSAuthn
  , msdb :: !Text
  , msdict :: !DbDict
  }
  deriving stock (TH.Lift, Show, Eq, Generic)

instance NFData a => NFData (DBMS a)

instance FromDhall (DBMS a) where
  autoWith _i = dbmssql

-- | decoder for a mssql
dbmssql :: Decoder (DBMS a)
dbmssql = genericAutoWith (fieldModDB "ms")

instance ToDhall MSAuthn where
  injectWith _ =
    adapt
      >$< unionEncoder
        ( encodeConstructorWith "Trusted" inject
            >|< encodeConstructorWith "UserPwd" (recordEncoder $ divide (\case UserPwd a b -> (a, b); o -> error ("invalid userpwd: found " ++ show o)) (encodeField @Text "user") (encodeField @Secret "password"))
        )
   where
    adapt Trusted = Left ()
    adapt z@UserPwd{} = Right z

instance ToDhall (DBMS a) where
  injectWith _o =
    recordEncoder $
      contramap
        (\(DBMS a b c d e) -> (a, (b, (c, (d, e)))))
        ( encodeField @Text "driver"
            >*< encodeField @Text "server"
            >*< encodeField @MSAuthn "authn"
            >*< encodeField @Text "db"
            >*< encodeField @DbDict "dict"
        )

instance ToText (DBMS a) where
  toText = fromText . msdb

instance DConn (DBMS a) where
  connList DBMS{..} =
    [ ("Driver", wrapOdbcBraces msdriver)
    , ("Server", msserver)
    , ("Database", msdb)
    ]
      <> connAuth msauthn
      <> unDict msdict
  getDbDefault _ = ''DBMS
  showDb DBMS{..} = [st|mssql ip=#{msserver} db=#{msdb}|]
  getSchema = const Nothing
  getDb = Just . msdb
  getDelims _ = ('[', ']') :| [('"', '"')]

-- | convert 'MSAuthn' to c# connection options
connAuth :: MSAuthn -> [(Text, Text)]
connAuth Trusted = [("Trusted_Connection", "yes")]
connAuth (UserPwd uid (Secret pwd)) = [("uid", uid), ("pwd", pwd)]
