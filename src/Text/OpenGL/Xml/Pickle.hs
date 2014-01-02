{-# OPTIONS_GHC -fno-warn-orphans #-}
module Text.OpenGL.Xml.Pickle (
) where

import Data.Char
import Data.List
import Data.List.Split
import Data.Monoid
import qualified Data.Set as S

import Text.ParserCombinators.ReadP
import Text.Read.Lex

import Text.XML.HXT.Core

import Text.OpenGL.Types

instance XmlPickler Registry where
    xpickle = xpElem "registry"
        $ xpWrap
            (\(tys, grs, ens, coms, feas, exts)
                -> Registry tys ens coms grs feas exts
            ,\(Registry tys ens coms grs feas exts)
                -> (tys, grs, ens, coms, feas, exts))
        $ xpFilterCont (hasNameWith $ (/=) "comment" . localPart)
        $ xp6Tuple
            (xpSetSquash $ xpElem "types" $ xpSet (xpickle :: PU Type))
            (xpSetSquash $ xpElem "groups" $ xpSet xpickle)
            (xpSet xpickle)
            (xpSet xpickle)
            (xpSet xpickle)
            (xpSetSquash $ xpElem "extensions" $ xpSet xpickle)
      where
        xpSetSquash :: Ord a =>  PU (S.Set a) -> PU (S.Set a)
        xpSetSquash = xpWrap (mconcat, (:[])) . xpList

instance XmlPickler Type where
    xpickle = xpElem "type"
        $ xpWrap
            ( \(api, req, name, com, impl) -> Type api req name com impl
            , \(Type api req name com impl) -> (api, req, name, com, impl))
        $ xp5Tuple
            (xpOption xpApiRef)
            (xpAttrImplied "requires" xpWText)
            xpWName
            xpComment
            xpickle
instance XmlPickler TypeImpl where
    xpickle = xpAlt tag ps
      where
        tag (TypeDef  _) = 0
        tag (BulkDefs _) = 1
        ps =
            [ xpWrap (TypeDef, \(TypeDef d) -> d) xpickle
--            , xpWrap (BulkDefs, \(BulkDefs d) -> d) $ xpElem "ctype" xpText
            , xpWrap (BulkDefs, \(BulkDefs d) -> d) $ xpElem "ctype" xpXmlText
            ]


instance XmlPickler Group where
    xpickle = xpElem "group"
        $ xpWrap (uncurry3 Group, \(Group n c es) -> (n,c,es))
        $ xpTriple
            (xpName (GroupName, \(GroupName n) -> n) )
            xpComment
            (xpWrap (S.fromList, S.toList) $ xpList
                $ xpElem "enum" xpWName)

instance XmlPickler Enums where
    xpickle = xpElem "enums"
        $ xpWrap
            ( \(ns, gr, ty, range, vn, com, enums')
                -> Enums ns gr ty range vn com enums'
            , \(Enums ns gr ty range vn com enums')
                -> (ns, gr, ty, range, vn, com, enums'))
        $ xp7Tuple
            (xpAttrImplied "namespace" xpText)
            (xpAttrImplied "group" xpWText)
            (xpAttrImplied "type" xpText)
            (xpOption xpickle)
            (xpAttrImplied "vendor" xpWText)
            xpComment
            xpEnums
      where
        xpEnums = xpSet (xpEither xpickle xpickle)

instance XmlPickler GLEnum where
    xpickle = xpElem "enum"
        $ xpWrap (\(v, ap, t, n, al, com) ->  Enum v ap t n al com
            , \(Enum v ap t n al com) -> (v, ap, t, n, al, com))
        $ xp6Tuple
            (xpAttr "value" xpInteger)
            (xpOption xpApiRef)
            (xpAttrImplied "type"
                $ xpWrap (TypeSuffix, \(TypeSuffix t) -> t) xpText)
            xpWName
            (xpAttrImplied "alias" xpWText)
            xpComment
instance XmlPickler Unused where
    xpickle =  xpElem "unused"
        $ xpWrap (uncurry Unused, \(Unused r c) -> (r,c))
        $ xpPair
            xpickle
            xpComment


instance XmlPickler Commands where
    xpickle = xpElem "commands"
        $ xpWrap (uncurry Commands, \(Commands ns cs) -> (ns, cs))
        $ xpPair
            (xpAttrImplied "namespace" xpText)
            (xpSet xpickle)

instance XmlPickler Command where
    xpickle = xpElem "command"
        $ xpWrap
            (\(name, ret, params, alias, vec, glx, com)
                -> Command name ret params alias vec glx com
            , \(Command name ret params alias vec glx com)
                -> (name, ret, params, alias, vec, glx, com))
        $ xp7Tuple
            xpWName
            xpickle
            (xpList xpickle)
            (xpOption $ xpFilterXpElems "alias"     $ xpWName)
            (xpOption $ xpFilterXpElems "vecequiv"  $ xpWName)
            (xpFilterElems "glx"  xpickle)
            xpComment

xpFilterElems :: String -> PU a -> PU a
xpFilterElems n = xpFilterCont (hasName n) -- TODO: make this work?

xpFilterXpElems :: String -> PU a -> PU a
xpFilterXpElems n = xpFilterElems n . xpElem n

instance XmlPickler ReturnType where
    xpickle = xpElem "return"
        $ xpWrap (uncurry ReturnType, \(ReturnType t gn) -> (t,gn))
        $ xpPair
            xpickle
            (xpAttrImplied "group" xpWText)
instance XmlPickler Param where
    xpickle = xpElem "param"
        $ xpWrap (uncurry4 Param, \(Param t name gr len) -> (t, name, gr, len))
        $ xp4Tuple
            xpickle
            (xpTextAttr "name")
            (xpAttrImplied "group" xpWText)
            (xpAttrImplied "len" xpText)
instance XmlPickler GlX where
    xpickle = xpElem "glx"
        $ xpWrap
            (uncurry4 GlX
            , \(GlX ty opcode name com) -> (ty, opcode, name, com))
        $ xp4Tuple
            (xpTextAttr "type")
            (xpAttr "opcode" xpickle)
            (xpAttrImplied "name" xpText)
            xpComment



instance XmlPickler Feature where
    xpickle = xpElem "feature"
        $ xpWrap
            (\(api, name, ver, prot, com, reqE, remE)
                -> Feature api name ver prot com reqE remE
            ,\(Feature api name ver prot com reqE remE)
                -> (api, name, ver, prot, com, reqE, remE))
        $ xp7Tuple
            xpApiRef
            xpWName
            (xpAttr "number" xpVersion)
            (xpAttrImplied "protect" xpWText)
            xpComment
            (xpFeatures "require")
            (xpFeatures "remove")
      where
        xpVersion :: PU (Int, Int)
        xpVersion = xpWrapEither (u, p) xpText
        u s = case break (== '.') s of
            (ma, '.':mi) | all isDigit ma && all isDigit mi
                -> Right (read ma, read mi) -- TODO don't use read?
            _   -> Left "Unparsed version number"
        p (ma,mi) = show ma ++ '.' : show mi

xpFeatures :: (Ord a, XmlPickler a) => String -> PU (S.Set a)
xpFeatures n = xpSet $ xpElem n xpickle

instance XmlPickler FeatureElement where
    xpickle = xpWrap
            ( \(prof, com, els) -> FeatureElement prof com els
            , \(FeatureElement prof com els) -> (prof, com, els))
        $ xpTriple
            (xpAttrImplied "profile" xpWText)
            xpComment
            (xpSet xpickle)

instance XmlPickler InterfaceElement where
    xpickle = xpAlt tag ps
      where
        tag t = case ieElementType t of
            IType _     -> 0
            IEnum _     -> 1
            ICommand _  -> 2
        ps =
            [ xp "type"     (IType, \(IType t) -> t)
            , xp "enum"     (IEnum, \(IEnum e) -> e)
            , xp "command"  (ICommand, \(ICommand c) -> c)
            ]
        xp :: TextWrapper w => String -> (w -> ElementType, ElementType ->  w) -> PU InterfaceElement
        xp name (f, g) = xpElem name
            $ xpWrap ( \(c, i) -> InterfaceElement c (f i)
                     , \(InterfaceElement c i) -> (c, g i))
            $ xpPair xpComment xpWName


instance XmlPickler Extension where
    xpickle = xpElem "extension"
        $ xpWrap
            ( \(name, prot, sup, com, reqE, remE)
                -> Extension name prot sup com reqE remE
            , \(Extension name prot sup com reqE remE)
                -> (name, prot, sup, com, reqE, remE))
        $ xp6Tuple
            xpWName
            (xpAttrImplied "protect" xpText)
            (xpAttrImplied "supported" xpWText)
            xpComment
            (xpFeatures "require")
            (xpFeatures "remove")


instance XmlPickler ExtensionElement where
    xpickle = xpWrap
            ( uncurry4 ExtensionElement
            ,\(ExtensionElement api prof com elems) -> (api, prof, com, elems))
        $ xp4Tuple
            (xpOption xpApiRef)
            (xpAttrImplied "profile" xpWText)
            xpComment
            (xpSet xpickle)


instance XmlPickler Range where
    xpickle = xpWrap (uncurry Range, \(Range s e) -> (s,e)) $
        xpPair (xpAttr "start" xpInteger) (xpAttrImplied "end" xpInteger)

instance XmlPickler Comment where
    xpickle = xpWrap (Comment, \(Comment c) -> c) $ xpTextAttr "comment"

xpSet :: Ord a => PU a -> PU (S.Set a)
xpSet = xpWrap (S.fromList, S.toList) . xpList

xpEither :: PU a -> PU b -> PU (Either a b)
xpEither p1 p2 = xpAlt tag ps
  where
    tag (Left _)  = 0
    tag (Right _) = 1
    ps = [ xpWrap (Left,  \(Left l)  -> l) p1
         , xpWrap (Right, \(Right r) -> r) p2
         ]

xpComment :: PU (Maybe Comment)
xpComment = xpOption xpickle

xpName :: (String -> n, n -> String) -> PU n
xpName fs = xpWrap fs $ xpTextAttr "name"

xpWName :: TextWrapper t => PU t
xpWName = xpName wrapper

xpWText :: TextWrapper t => PU t
xpWText = xpWrap wrapper xpText

xpApiRef :: PU ApiReference
xpApiRef = xpWrap wrapper $ xpTextAttr "api"

xpInteger :: PU Integer
xpInteger = xpWrapEither (readNum, show) xpText
  where
    readNum s = case s of
        '-':rs      -> negate `fmap` readNum rs
        '0':'x':rs  -> readWith readHexP rs
        rs          -> readWith readDecP rs
    readWith rp s = case (readP_to_S rp) s of
        [(num,"")]  -> Right num
        _           -> Left $ "ambiguous or malformed number: " ++ s

class TextWrapper t where
    wrapper :: (String -> t, t -> String)

instance TextWrapper TypeName where
    wrapper = (TypeName, \(TypeName t) -> t)
instance TextWrapper EnumName where
    wrapper = (EnumName, \(EnumName n) -> n)
instance TextWrapper CommandName where
    wrapper = (CommandName, \(CommandName c) -> c)

instance TextWrapper TypeSuffix where
    wrapper = (TypeSuffix, \(TypeSuffix s) -> s)
instance TextWrapper ProfileName where
    wrapper = (ProfileName, \(ProfileName p) -> p)
instance TextWrapper GroupName where
    wrapper = (GroupName, \(GroupName g) -> g)
instance TextWrapper CPPToken where
    wrapper = (CPPToken, \(CPPToken c) -> c)
instance TextWrapper VendorName where
    wrapper = (VendorName, \(VendorName v) -> v)

instance TextWrapper ApiReference where
    wrapper = (u,p)
      where
        u str = case str of
            "gl"    -> GL
            "gles1" -> GLES1
            "gles2" -> GLES2
            _       -> OtherApi str
        p val = case val of
            GL          -> "gl"
            GLES1       -> "gles1"
            GLES2       -> "gles2"
            OtherApi n  -> n
instance TextWrapper StringGroup where
    wrapper = (u,p)
      where
        u = StringGroup . map (fst wrapper) . splitOn "|"
        p = intercalate "|" . map (snd wrapper) . (\(StringGroup g) -> g)
