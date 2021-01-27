-- need to add this to dictionary in dhall option=67108864
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

Implementation of GConn for mysql.
-}
module Database.MySql where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (st,ToText(..))
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Dhall
    ( Natural,
      (>*<),
      defaultInterpretOptions,
      encodeField,
      genericAutoWith,
      recordEncoder,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import Database.Util
import Data.Maybe (fromMaybe)
import Data.Functor.Contravariant
import Control.DeepSeq (NFData)

data DBMY a =
  DBMY
    { mydriver :: !Text
    , myserver :: !Text
    , myuid :: !Text
    , mypwd :: !Secret
    , mydb :: !Text
    , myport :: !(Maybe Natural)
    , mydict :: !DbDict
    } deriving (TH.Lift, Show, Generic, Eq)

instance NFData a => NFData (DBMY a)

instance FromDhall (DBMY a) where
  autoWith _i = dbmysql

dbmysql :: Decoder (DBMY a)
dbmysql = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop (T.length "my") }

instance ToDhall (DBMY a) where
  injectWith _o = recordEncoder $ contramap (\(DBMY a b c d e f g) -> (a, (b, (c, (d, (e, (f, g)))))))
         (encodeField @Text "driver" >*<
         encodeField @Text "server" >*<
         encodeField @Text "uid" >*<
         encodeField @Secret "pwd" >*<
         encodeField @Text "db" >*<
         encodeField @(Maybe Natural) "port" >*<
         encodeField @DbDict "dict")


instance ToText (DBMY a) where
  toText = fromText . mydb

-- {mydriver};Server=#{myserver};Port=#{maybe "3306" show myport};Database=#{mydb};User=#{myuid};Password=#{unSecret mypwd};option=67108864;#{extra}|]

instance DConn (DBMY a) where
  connList DBMY {..} =
    [ ("Driver", wrapBraces mydriver)
    , ("Server", myserver)
    , ("Port", T.pack (show (fromMaybe 3306 myport)))
    , ("Database", mydb)
    , ("User", myuid)
    , ("Password", unSecret mypwd)
    ] <> unDict mydict
  getDbDefault _ = ''DBMY
  showDb DBMY {..} = [st|mysql ip=#{myserver} db=#{mydb}|]
  getSchema = Just . mydb -- no schemas within dbs ie treats dbs as if it is a schema!!!
  getDb = const Nothing
  getDelims _ = Just ('`','`')
