{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.OpenGL.Parser where

import Control.Applicative
import Control.Monad.Reader
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Traversable
import Text.XML.Light

import Text.OpenGL.Types

type P = ReaderT Element (Either String)

parse :: Element -> P Registry
parse = undefined

pRegistry :: P Registry
pRegistry = Registry
    <$> pMergeChildren  "types"         pTypes
    <*> pChildrenS      "enums"         pEnums
    <*> pMergeChildren  "commands"      pCommands
    <*> pMergeChildren  "groups"        pGroups
    <*> pChildrenS      "feature"       pFeature
    <*> pMergeChildren  "extensions"    pExtensions

pTypes :: P (Set Type)
pTypes = pChildrenS "type" pType

pType :: P Type
pType = undefined

pGroups :: P (Set Group)
pGroups = pChildrenS "group" pGroup

pGroup :: P Group
pGroup = undefined

pEnums :: P Enums
pEnums = undefined

pCommands :: P (Set Command)
pCommands = pChildrenS "command" pCommand

pCommand :: P Command
pCommand = undefined

pFeature :: P Feature
pFeature = undefined

pExtensions :: P (Set Extension)
pExtensions = pChildrenS "extension" pExtension

pExtension :: P Extension
pExtension = undefined

pMergeChildren :: Monoid a => QName -> P a -> P a
pMergeChildren name f = mconcat <$> pChildren name f

pChildren :: QName -> P a -> P [a]
pChildren name f = asks (findChildren name) >>= traverse (flip withElement f)

pChildrenS :: Ord a => QName -> P a -> P (Set a)
pChildrenS name f = S.fromList <$> pChildren name f

withElement :: Element -> P a -> P a
withElement = local . const

instance IsString QName where
    fromString = unqual
