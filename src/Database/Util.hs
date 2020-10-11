{-# LANGUAGE TupleSections #-}
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
import Data.String (IsString(..))
import GHC.Generics (Generic)
import Dhall (FromDhall(..), ToDhall(..))
import qualified Language.Haskell.TH.Syntax as TH (Lift,Name)
import Data.Aeson (ToJSON(..))
import Data.Functor.Contravariant ((>$<))
import GHC.Stack (HasCallStack)
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
wrapBraces (T.strip -> x) =
  let msg = "dont use braces and dont use Driver=... eg this works: \"ODBC Driver 17 for SQL Server\""
      msg0 = "\n" ++ msg ++ "\n[" ++ T.unpack x ++ "]"
  in case T.uncons x of
       Nothing -> error "wrapBraces: missing driver!!"
       Just (s,y) | T.length x < 5 -> error $ "wrapBraces: not enough characters" ++ msg0
                  | otherwise ->
         case T.unsnoc y of
           Nothing -> error $ "wrapBraces: not enough characters" ++ msg0
           Just (_,e) ->
             case (s,e) of
               ('{','}') -> x
               ('{',_) -> error $ "wrapBraces: found open brace and without a closing brace" ++ msg0
               (_,'}') -> error $ "wrapBraces: found closed brace and without an opening brace" ++ msg0
               _ -> "{" <> x <> "}"


