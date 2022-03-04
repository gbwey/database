{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      : Database.Util
Description : Utility methods
Copyright   : (c) Grant Weyburne, 2016
License     : GPL-3

Mainly has various logging functions and timing of commands.
Allows you to log to a file or the screen or both
-}
module Database.Util (
  DConn (..),
  DbDict (..),
  wrapOdbcBraces,
  fieldModDB,
  Secret (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON (..))
import Data.Functor.Contravariant ((>$<))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N
import Data.Maybe
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Dhall (FromDhall (..), ToDhall (..))
import qualified Dhall as D
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import qualified Language.Haskell.TH.Syntax as TH (Lift, Name)

-- | simple accessor class for database connections
class DConn a where
  connList :: a -> [(Text, Text)]
  connText :: a -> Text
  connText a = T.intercalate ";" (map (\(k, v) -> k <> "=" <> v) (connList a))
  getDbDefault :: HasCallStack => p a -> TH.Name
  showDb :: a -> Text
  getDb :: a -> Maybe Text
  getSchema :: a -> Maybe Text

  -- | start and end deimiters for each database type
  getDelims :: HasCallStack => proxy a -> NonEmpty (Char, Char)

-- | dictionary holding odbc options
newtype DbDict = DbDict {unDict :: [(Text, Text)]}
  deriving stock (TH.Lift, Generic, Eq, Show)
  deriving newtype (Semigroup, Monoid)

instance NFData DbDict

instance ToDhall DbDict where
  injectWith i = unDict >$< injectWith i

instance FromDhall DbDict where
  autoWith i = DbDict <$> autoWith i

-- | hides the password
newtype Secret = Secret {unSecret :: Text} deriving stock (TH.Lift, Generic, Eq)

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

-- | validates an odbc connection string and wraps braces as needed
wrapOdbcBraces :: HasCallStack => Text -> Text
wrapOdbcBraces (T.strip -> x) =
  let msg1 :: String
      msg1 = "dont use braces and dont use Driver=... eg this works: \"ODBC Driver 17 for SQL Server\""
      f txt = "wrapOdbcBraces:" ++ txt ++ "\n" ++ msg1 ++ "\n[" ++ T.unpack x ++ "]"
   in case T.unpack x of
        s : xs@(_ : _ : _ : _ : _) ->
          case (s, N.last (s :| xs)) of
            ('{', '}') -> x
            ('{', _) -> error $ f "found an open brace without a closing brace"
            (_, '}') -> error $ f "found a closed brace without an opening brace"
            (_, _) -> "{" <> x <> "}"
        _o -> error $ f "not enough characters"

{- | translates a haskell field to the dhall field without the prefix
   checks that the given a prefix matches for each of the fields
-}
fieldModDB :: HasCallStack => Text -> D.InterpretOptions
fieldModDB prefix =
  D.defaultInterpretOptions
    { D.fieldModifier = f
    }
 where
  f field =
    fromMaybe
      (error $ T.unpack $ "fieldModDB:expected prefix[" <> prefix <> "] for field[" <> field <> "]")
      (T.stripPrefix prefix field)
