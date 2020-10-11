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
import Control.Lens.TH (makeLenses, makePrisms)
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

data MSAuthn = Trusted | UserPwd { _msuser :: !Text, _mspassword :: !Secret }
  deriving (TH.Lift, Show, Eq, Generic, Read)

makePrisms ''MSAuthn

instance NFData MSAuthn

instance FromDhall MSAuthn where
  autoWith _i = genericAutoWith (defaultInterpretOptions { fieldModifier = T.drop 3 })

data DBMS a =
  DBMS
    { _msdriver :: !Text
    , _msserver :: !Text
    , _msauthn :: !MSAuthn
    , _msdb :: !Text
    , _msdict :: !DbDict
    } deriving (TH.Lift, Show, Eq, Generic, Read)

makeLenses ''DBMS

instance NFData a => NFData (DBMS a)

instance FromDhall (DBMS a) where
  autoWith _i = dbmssql

dbmssql :: Decoder (DBMS a)
dbmssql = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 3 }

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
  toText = fromText . _msdb

instance DConn (DBMS a) where
  connList DBMS {..} =
    [ ("Driver", wrapBraces _msdriver)
    , ("Server", _msserver)
    , ("Database", _msdb)
    ] <> connAuth _msauthn
      <> unDict _msdict
  getDbDefault _ = ''DBMS
  showDb DBMS {..} = [st|mssql ip=#{_msserver} db=#{_msdb}|]
  getSchema = const Nothing
  getDb = Just . _msdb
  getDelims _ = Just ('[',']')

connAuth :: MSAuthn -> [(Text, Text)]
connAuth Trusted = [("Trusted_Connection","yes")]
connAuth (UserPwd uid (Secret pwd)) = [("uid", uid), ("pwd", pwd)]

