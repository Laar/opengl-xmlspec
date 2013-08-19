{-# LANGUAGE OverloadedStrings#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.OpenGL.Parser (
    parseRegistry
) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Reader
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import Data.Traversable
import Text.XML.Light

import Text.ParserCombinators.ReadP
import Text.Read.Lex

import Text.OpenGL.Types

type P = ReaderT Element (Either String)

parseRegistry :: Element -> Either String Registry
parseRegistry = runReaderT pRegistry

pRegistry :: P Registry
pRegistry = Registry
    <$> pMergeChildren  "types"         pTypes
    <*> pChildrenS      "enums"         pEnums
    <*> pChildrenS      "commands"      pCommands
    <*> pMergeChildren  "groups"        pGroups
    <*> pChildrenS      "feature"       pFeature
    <*> pMergeChildren  "extensions"    pExtensions

--

pTypes :: P (Set Type)
pTypes = pChildrenS "type" pType

pType :: P Type
pType
    = Type
    <$> pOptApiReference
    <*> asksOptAttr "requires" TypeName
    <*> pTypeName
    <*> pComment
    <*> pTypeImple
    where
        pTypeName :: P TypeName
        pTypeName = pName TypeName <|> pChild "name" (TypeName <$> pStrContent)
        pTypeImple = return $ BulkDefs "JUNK JUNK JUNK" -- TODO Implement

--

pGroups :: P (Set Group)
pGroups = pChildrenS "group" pGroup

pGroup :: P Group
pGroup
    = Group
    <$> pName GroupName
    <*> pComment
    <*> pChildrenS "enum" (pName EnumName)

--

pEnums :: P Enums
pEnums
    = Enums
    <$> askOptAttr  "namespace"
    <*> asksOptAttr "group" GroupName
    <*> askOptAttr  "type"
    <*> pRange
    <*> asksOptAttr "vendor" VendorName
    <*> pComment
    <*> pTryChildrenS ((Left <$> pEnum) <|> (Right <$> pUnused))

pRange :: P (Maybe Range)
pRange = asks $ \e -> case findAttr "start" e of
    Nothing -> Nothing
    Just s  -> Just $ Range (textToInt s) (textToInt <$> findAttr "end" e)

pEnum :: P GLEnum
pEnum
    = guardName "enum"
    $ Enum
    <$> asksAttr "value" textToInt
    <*> pOptApiReference
    <*> asksOptAttr "type" TypeSuffix
    <*> pName EnumName
    <*> asksOptAttr "alias" EnumName
    <*> pComment

pUnused :: P Unused
pUnused
    = guardName "unused"
    $ Unused
    <$> require pRange
    <*> pComment

textToInt :: String -> Integer
textToInt source = case source of
    '0':'x':s -> parseBase readHexP s
    s         -> parseBase readDecP s
    where
        parseBase :: ReadP a -> String -> a
        parseBase readP s = case filter (null . snd) $ readP_to_S readP s of
            [(x,_)] -> x
            _ -> error "error" -- TODO handle

--

pCommands :: P Commands
pCommands
    = Commands
    <$> askOptAttr "namespace"
    <*> pChildrenS "command" pCommand

pCommand :: P Command
pCommand
    = Command
    <$> (pChild "proto" . pChild "name" $ CommandName <$> pStrContent)
    <*> return Void    -- TODO: returnType
    <*> return Nothing -- TODO: groupName
    <*> pChildren "param" pParam
    <*> asksOptAttr "alias" CommandName
    <*> asksOptAttr "vecequiv" CommandName
    <*> pChildren "glx" pGlX
    where
        pGlX = GlX  <$> askAttr "type"    <*> (read <$> askAttr "opcode")
                    <*> askOptAttr "name" <*> pComment
        pParam = return Param -- TODO

--

pFeature :: P Feature
pFeature
    = Feature
    <$> pApiReference
    <*> pName CPPToken
    <*> asksAttr "number" read -- TODO use something beter than read
    <*> asksOptAttr "protect" CPPToken
    <*> pComment
    <*> pChildrenS "require" pFeatureElement
    <*> pChildrenS "remove"  pFeatureElement

pFeatureElement :: P FeatureElement
pFeatureElement
    = FeatureElement
    <$> pOptProfileName
    <*> pComment
    <*> pTryChildrenS pInterfaceElement

pInterfaceElement :: P InterfaceElement
pInterfaceElement
    = InterfaceElement
    <$> pComment
    <*> (   guardName "type"    (pName $ IType . TypeName)
        <|> guardName "enum"    (pName $ IEnum . EnumName)
        <|> guardName "command" (pName $ ICommand . CommandName)
        )

--

pExtensions :: P (Set Extension)
pExtensions = pChildrenS "extension" pExtension

pExtension :: P Extension
pExtension
    = Extension
    <$> pName CPPToken
    <*> askOptAttr "protect"
    <*> asksOptAttr "supported" StringGroup
    <*> pComment
    <*> pChildrenS "require" pExtensionElement
    <*> pChildrenS "remove"  pExtensionElement

pExtensionElement :: P ExtensionElement
pExtensionElement
    = ExtensionElement
    <$> pOptApiReference
    <*> pOptProfileName
    <*> pComment
    <*> pTryChildrenS pInterfaceElement


--

pName :: (String -> a) -> P a
pName f = f <$> askAttr "name"

pComment :: P (Maybe Comment)
pComment = asksOptAttr "comment" Comment

pApiReference :: P ApiReference
pApiReference = asksAttr "api" ApiReference

pOptApiReference :: P (Maybe ApiReference)
pOptApiReference = asksOptAttr "api" ApiReference

pOptProfileName :: P (Maybe ProfileName)
pOptProfileName = asksOptAttr "profile" ProfileName

--

pMergeChildren :: Monoid a => QName -> P a -> P a
pMergeChildren name f = mconcat <$> pChildren name f

pChildren :: QName -> P a -> P [a]
pChildren name f = asks (findChildren name) >>= traverse (flip withElement f)

pChildrenS :: Ord a => QName -> P a -> P (Set a)
pChildrenS name f = S.fromList <$> pChildren name f

pChild :: QName -> P a -> P a
pChild qn f = asks (findChildren qn) >>= \children -> case children of
    []  -> throwError $ "No children named " ++ show qn
    [e] -> withElement e f
    _   -> throwError $ "Multiple children named " ++ show qn

pStrContent :: P String
pStrContent = asks strContent

pTryChildren :: P a -> P [a] -- TODO make a more readable implementation
pTryChildren f = asks (elChildren) >>= \es -> fmap concat . for es $ \e ->
    ((:[]) <$> withElement e f) `catchError` \_ -> return []

pTryChildrenS :: Ord a => P a -> P (Set a)
pTryChildrenS f = S.fromList <$> pTryChildren f

guardName :: QName -> P a -> P a
guardName qn p = asks elName >>= \name ->
    if name == qn
     then p
     else throwError $ "Name mismatch, expected " ++ show qn ++ ", received " ++ show name

askAttr :: QName -> P String
askAttr qn = askOptAttr qn >>= maybe (throwError $ "no attribute named " ++ show qn) return

asksAttr :: QName -> (String -> a) -> P a
asksAttr qn f = f <$> askAttr qn

askOptAttr :: QName -> P (Maybe String)
askOptAttr = asks . findAttr

asksOptAttr :: QName -> (String -> a) -> P (Maybe a)
asksOptAttr qn f = fmap f <$> askOptAttr qn

require :: P (Maybe a) -> P a
require p = p >>= \r -> case r of
    Just r' -> return r'
    Nothing ->  throwError "Requirement failed"


withElement :: Element -> P a -> P a
withElement = local . const

instance IsString QName where
    fromString = unqual
