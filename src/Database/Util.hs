{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
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
import qualified Control.Monad.State.Strict as S
--import Control.Lens
import Data.String
import qualified GHC.Generics as G
import Dhall hiding (string,auto,map)
--import qualified Dhall as D
--import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
--import Data.Char
--import Data.List
import Data.Aeson (ToJSON(..))
import Data.Functor.Contravariant ((>$<))

class DConn a where
  connText :: a -> Text
  getDbDefault :: p a -> TH.Name

newtype Secret = Secret { unSecret :: Text } deriving (TH.Lift, Generic, Eq, Read)

instance IsString Secret where
  fromString = Secret . T.pack

instance ToDhall Secret where
  injectWith i = unSecret >$< injectWith @Text i

instance FromDhall Secret where
  autoWith i = Secret <$> autoWith @Text i

instance ToJSON Secret where
  toJSON (Secret s) = toJSON s

instance Show Secret where
  show _ = "Secret *******"

genericAutoY :: (Generic a, GenericFromDhall (G.Rep a)) => InterpretOptions -> Type a
genericAutoY i = fmap G.to (S.evalState (genericAutoWith i) 1)


