{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
{- |
Module      : Database
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3
Maintainer  : gbwey9@gmail.com

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Database.Util where
import qualified Data.Text as T
import Data.Text (Text)
import Data.String
import Dhall hiding (string,auto,map)
import qualified Language.Haskell.TH.Syntax as TH (Lift,Name)
import Data.Aeson (ToJSON(..))
import Data.Functor.Contravariant ((>$<))
import GHC.Stack
import Control.DeepSeq (NFData)

class DConn a where
  connList :: HasCallStack => a -> [(Text, Text)]
  connText :: HasCallStack => a -> Text
  connText a = T.intercalate ";" (map (\(k, v) -> k <> "=" <> v) (connList a))
  getDbDefault :: HasCallStack => p a -> TH.Name
  showDb :: a -> Text
  getDb :: a -> Maybe Text
  getSchema :: a -> Maybe Text
  -- | start and end deimiters for each database type
  getDelims :: HasCallStack => proxy a -> Maybe (Char, Char)

newtype DbDict = DbDict { unDict :: [(Text, Text)] } deriving (TH.Lift, Generic, Eq, Read, Show)
instance NFData DbDict

instance Semigroup DbDict where
  DbDict a <> DbDict b = DbDict (a <> b)

instance Monoid DbDict where
  mempty = DbDict mempty

instance ToDhall DbDict where
  injectWith i = unDict >$< injectWith i

instance FromDhall DbDict where
  autoWith i = DbDict <$> autoWith i

newtype Secret = Secret { unSecret :: Text } deriving (TH.Lift, Generic, Eq, Read)
instance NFData Secret

instance IsString Secret where
  fromString = Secret . T.pack

instance ToDhall Secret where
  injectWith i = unSecret >$< injectWith @Text i

instance FromDhall Secret where
  autoWith i = Secret <$> autoWith @Text i

instance ToJSON Secret where
  toJSON (Secret s) = toJSON s

instance Show Secret where
  show _ = "Secret ********"

wrapBraces :: HasCallStack => Text -> Text
wrapBraces (T.strip -> x)
  | T.null x = error "wrapBraces: missing driver!!"
  | otherwise =
    let msg = "dont use braces and dont use Driver=... eg this works: \"ODBC Driver 17 for SQL Server\""
    in case (T.head x, T.last x) of
      ('{','}') -> x
      ('{',_) -> error $ "wrapBraces: found open brace and without a closing brace:\n" ++ msg ++ "\n[" ++ T.unpack x ++ "]"
      (_,'}') -> error $ "wrapBraces: found closed brace and without an open brace:\n" ++ msg ++ "\n[" ++ T.unpack x ++ "]"
      _ -> "{" <> x <> "}"


