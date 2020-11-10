-- todo: fix ToText instance
-- todo: orschema was never used but we dont use it in the connection string but is required downstream for getalltables etc
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

Implementation of GConn for oracle.
-}
module Database.Oracle where
import Prelude hiding (FilePath)
import Text.Shakespeare.Text (ToText(..), st)
import Data.Text.Lazy.Builder (fromText)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Lens.TH (makeLenses, makePrisms)
import qualified Language.Haskell.TH.Syntax as TH (Lift)
import Dhall
    ( (>*<),
      (>|<),
      constructor,
      defaultInterpretOptions,
      encodeConstructorWith,
      encodeField,
      field,
      genericAutoWith,
      inject,
      record,
      recordEncoder,
      union,
      unionEncoder,
      Decoder,
      FromDhall(..),
      InterpretOptions(fieldModifier),
      ToDhall(..) )
import qualified Dhall as D
import Database.Util
import Data.Functor.Contravariant ((>$<), Contravariant(contramap))
import Data.Functor.Contravariant.Divisible (Divisible(divide))
import Control.DeepSeq (NFData)

data OracleConnType = TnsName { _ocdriver :: !Text, _octns :: !Text } | DsnOracle !Text
  deriving (TH.Lift, Show, Generic, Read, Eq)

makePrisms ''OracleConnType

instance NFData OracleConnType

instance FromDhall OracleConnType where
  autoWith _ = toOCT

-- union of a record and a single constructor
-- constructor is a functor only but record is applicative
toOCT :: D.Decoder OracleConnType
toOCT = union
  (  constructor "TnsName" (record ( TnsName <$> field "driver" D.strictText <*> field "tns" D.strictText ))
  <> ( DsnOracle <$> constructor "DsnOracle" D.strictText)
  )

data DBOracle a =
  DBOracle
    { _orConnType :: !OracleConnType
    , _oruid :: !Text
    , _orpwd :: !Secret
    , _orschema :: !Text
    , _ordict :: !DbDict
    } deriving (Eq, TH.Lift, Show, Generic, Read)

makeLenses ''DBOracle

instance NFData a => NFData (DBOracle a)

getOrconnTypeList :: OracleConnType -> [(Text, Text)]
getOrconnTypeList = \case
  TnsName driver tns -> [("Driver", wrapBraces driver), ("dbq", tns)]
  DsnOracle dsn -> [("DSN", dsn)]

instance FromDhall (DBOracle a) where
  autoWith _i = dboracle

dboracle :: Decoder (DBOracle a)
dboracle = genericAutoWith defaultInterpretOptions { fieldModifier = T.drop 3 }

instance ToText OracleConnType where
  toText = fromText . T.pack . show

instance ToDhall OracleConnType where
  injectWith _ = adapt >$< unionEncoder
   (   encodeConstructorWith "DsnOracle" (inject @Text)
   >|< encodeConstructorWith "TnsName" (recordEncoder $ divide (\case TnsName a b -> (a,b); o -> error ("invalid tnsname: found " ++ show o) ) (encodeField @Text "driver") (encodeField @Text "tns"))
   )
   where
     adapt (DsnOracle a) = Left a
     adapt (TnsName a b) = Right (TnsName a b)

instance ToDhall (DBOracle a) where
  injectWith _o = recordEncoder $ contramap (\(DBOracle a b c d e) -> (a, (b, (c, (d, e)))))
         (encodeField @OracleConnType "ConnType" >*<
         encodeField @Text "uid" >*<
         encodeField @Secret "pwd" >*<
         encodeField @Text "schema" >*<
         encodeField @DbDict "dict")

instance ToText (DBOracle a) where
  toText = fromText . showDb

instance DConn (DBOracle a) where
  connList DBOracle {..} =
--    getOrconnTypeList _orConnType <> (maybe [] (\x -> [("Schema", x)]) _orschema) <> [("Uid", _oruid), ("Pwd", unSecret _orpwd)] <> M.toList _ordict
    getOrconnTypeList _orConnType
     <> [("Uid", _oruid), ("Pwd", unSecret _orpwd)]
     <> unDict _ordict
  getDbDefault _ = ''DBOracle
  showDb DBOracle {..} = [st|oracle #{_orConnType} schema=#{_orschema}|]
  getSchema = Just . _orschema
  getDb = const Nothing -- i dont know how to go across dbs within oracle
  getDelims _ = Just ('\"','\"')
