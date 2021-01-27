{-# OPTIONS -Wno-partial-fields #-}
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
import Text.Shakespeare.Text (st,ToText(..))
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Dhall
    ( (>*<),
      (>|<),
      defaultInterpretOptions,
      encodeConstructorWith,
      encodeField,
      genericAutoWith,
      inject,
      recordEncoder,
      unionEncoder,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import Database.Util
import Data.Functor.Contravariant ((>$<), Contravariant(contramap))
import Data.Functor.Contravariant.Divisible
import Control.DeepSeq (NFData)

data MSAuthn = Trusted | UserPwd { msuser :: !Text, mspassword :: !Secret }
  deriving (TH.Lift, Show, Eq, Generic)

instance NFData MSAuthn

instance FromDhall MSAuthn where
  autoWith _i = genericAutoWith (defaultInterpretOptions { fieldModifier = T.drop (T.length "ms") })

data DBMS a =
  DBMS
    { msdriver :: !Text
    , msserver :: !Text
    , msauthn :: !MSAuthn
    , msdb :: !Text
    , msdict :: !DbDict
    } deriving (TH.Lift, Show, Eq, Generic)

instance NFData a => NFData (DBMS a)

instance FromDhall (DBMS a) where
  autoWith _i = dbmssql

dbmssql :: Decoder (DBMS a)
dbmssql = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "ms") }

instance ToDhall MSAuthn where
  injectWith _ = adapt >$< unionEncoder
   (   encodeConstructorWith "Trusted" inject
   >|< encodeConstructorWith "UserPwd" (recordEncoder $ divide (\case UserPwd a b -> (a,b); o -> error ("invalid userpwd: found " ++ show o)) (encodeField @Text "user") (encodeField @Secret "password"))
   )
   where
     adapt Trusted = Left ()
     adapt z@UserPwd {} = Right z

instance ToDhall (DBMS a) where
  injectWith _o = recordEncoder $ contramap (\(DBMS a b c d e) -> (a, (b, (c, (d, e)))))
         (encodeField @Text "driver" >*<
         encodeField @Text "server" >*<
         encodeField @MSAuthn "authn" >*<
         encodeField @Text "db" >*<
         encodeField @DbDict "dict")

instance ToText (DBMS a) where
  toText = fromText . msdb

instance DConn (DBMS a) where
  connList DBMS {..} =
    [ ("Driver", wrapBraces msdriver)
    , ("Server", msserver)
    , ("Database", msdb)
    ] <> connAuth msauthn
      <> unDict msdict
  getDbDefault _ = ''DBMS
  showDb DBMS {..} = [st|mssql ip=#{msserver} db=#{msdb}|]
  getSchema = const Nothing
  getDb = Just . msdb
  getDelims _ = Just ('[',']')

connAuth :: MSAuthn -> [(Text, Text)]
connAuth Trusted = [("Trusted_Connection","yes")]
connAuth (UserPwd uid (Secret pwd)) = [("uid", uid), ("pwd", pwd)]

