module Text.OpenGL.Xml.TypeTypes (
    TypeDef(..),
    CType(..),
    BaseType(..),
) where

import Control.Applicative
import Text.XML.HXT.Core

data TypeDef
    = Alias CType
    | FuncDef CType [(CType, String)]
    | StructDef String
    deriving (Eq, Ord, Show)

data CType
    = Struct    String
    | CConst    CType
    | Ptr       CType
    | BaseType  BaseType
    | AliasType String
    deriving (Eq, Ord, Show)

data BaseType
    = Void
    | Char
    | Short
    | Int
    | Int32T
    | Int64T
    | UInt64T
    | Long
    | PtrdiffT
    | SizeT
    | Float
    | Double
    | Signed BaseType
    | Unsigned BaseType
    deriving (Eq, Ord, Read, Show)

instance XmlPickler BaseType where
    xpickle = xpWrapEither (up, p) $ xpTextAttr "name"
      where
        p bt = case bt of
            Void        -> "void"
            Char        -> "char"
            Short       -> "short"
            Int         -> "int"
            Int32T      -> "int32_t"
            Int64T      -> "int64_t"
            UInt64T     -> "uint64_t"
            Long        -> "long"
            PtrdiffT    -> "ptrdiff_t"
            SizeT       -> "size_t"
            Float       -> "float"
            Double      -> "double"
            Signed t    -> "signed " ++ p t
            Unsigned t  -> "unsigned " ++ p t
        up s = case s of
            "void"      -> Right Void
            "char"      -> Right Char
            "short"     -> Right Short
            "int"       -> Right Int
            "int32_t"   -> Right Int32T
            "int64_t"   -> Right Int64T
            "uint64_t"  -> Right UInt64T
            "long"      -> Right Long
            "ptrdiff_t" -> Right PtrdiffT
            "size_t"    -> Right SizeT
            "float"     -> Right Float
            "double"    -> Right Double
            's':'i':'g':'n':'e':'d':' ':rs -> Signed <$> up rs
            'u':'n':'s':'i':'g':'n':'e':'d':' ':rs -> Unsigned <$> up rs
            _           -> Left $ "Unknown type \"" ++ s ++ "\""

instance XmlPickler TypeDef where
    xpickle = xpAlt tag ps
      where
        tag (Alias _)       = 0
        tag (FuncDef _ _)   = 1
        tag (StructDef _)   = 2
        ps =
            [ xpWrap (Alias, \(Alias a) -> a) $
                xpElem "alias" $ xpickle
            , xpWrap (uncurry FuncDef, \(FuncDef r as) -> (r,as)) $
                xpPair
                    (xpElem "return" xpickle)
                    (xpList $ xpElem "param" $ xpPair xpickle (xpTextAttr "name"))
            , xpWrap (StructDef, \(StructDef s) -> s) $
                xpElem "structdef" $ xpTextAttr "name"
            ]

instance XmlPickler CType where
    xpickle = xpAlt tag ps
      where
        tag (Struct _)      = 0
        tag (CConst _)      = 1
        tag (Ptr _)         = 2
        tag (BaseType _)    = 3
        tag (AliasType _)   = 4
        ps =
            [ xpWrap (Struct, \(Struct s) -> s) $
                xpElem "struct" $ xpTextAttr "name"
            , xpWrap (CConst, \(CConst t) -> t) $
                xpElem "const" $ xpickle
            , xpWrap (Ptr, \(Ptr t) -> t) $
                xpElem "ptr" xpickle
            , xpWrap (BaseType, \(BaseType b) -> b) $
                xpElem "basetype" $ xpickle
            , xpWrap (AliasType, \(AliasType n) -> n) $
                xpElem "aliastype" $ xpTextAttr "name"
            ]
