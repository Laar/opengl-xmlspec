module Text.OpenGL.Types (
) where

import Data.Set (Set)

-- TODO: registry

data Types
    = Types
    { types :: Set Type
    } deriving (Eq, Ord, Show)

data Type
    = Type
    { typeApi       :: Maybe ApiReference
    , typeRequires  :: Maybe String
    , typeName      :: Maybe String
    , typeComment   :: Maybe Comment
    , typeImpl      :: TypeImpl
    } deriving (Eq, Ord, Show)

-- This probably needs some tweaking...
data TypeImpl
    -- | The basic type defs, e.g. 'typedef int <name>GLint</name>;'
    = TypeDef   String Name
    -- | These types are not that easy so for now just all the parts.
    | ApiEntry  String String String String
    -- | Some types
    | BulkDefs  String

data Enums
    = Enums
    { enumsNamespace    :: Maybe String
    , enumsClass        :: Maybe String
    , enumsType         :: Maybe String
    , enumsRange        :: Maybe Range
    , enumsVendor       :: Maybe String
    , enumsComment      :: Maybe Comment
    , enums             :: Set (Either Enum Unused)
    } deriving (Eq, Ord, Show)

data Enum
    = Enum
    { value     :: Integer
    , enumApi   :: Maybe ApiReference
    , enumType  :: Maybe TypeSuffix
    , name      :: String
    , alias     :: Maybe String
    } deriving (Eq, Ord, Show)

data Unused
    = Unused
    { unusedRange :: Range
    , comment   :: Maybe Comment
    } deriving (Eq, Ord, Show)

data Commands
    = Commands
    { namespace :: Maybe String
    , commands  :: Set Command
    } deriving (Eq, Ord, Show)

data Command
    = Command
    { proto     :: Proto
    , params    :: [Param]
    , alias     :: Maybe Name -- inconsistent with alias from enum
    , vecequiv  :: Maybe Name
    , glx       :: [GlX]
    } deriving (Eq, Ord, Show)

data Proto
    = Proto
    { -- TODO: interpret & implement
    } deriving (Eq, Ord, Show)

data Param
    = Param
    { -- TODO: interpret & implement
    } deriving (Eq, Ord, Show)

data GlX
    = GlX
    { glXType   :: String
    , opCode    :: Int
    , name      :: Maybe Name
    , comment   :: Maybe Comment
    } deriving (Eq, Ord, Show)

data Feature
    = Feature
    { featureApi    :: ApiReference
    , name      :: Name
    , number    :: Float -- TODO: this seems to be a version
    , protect   :: String
    , comment   :: Maybe String
    , requires  :: Set FeatureElement
    , removes   :: Set FeatureElement
    } deriving (Eq, Ord, Show)

data FeatureElement
    = FeatureElement
    { profileName   :: Maybe ProfileName
    , comment       :: Maybe Comment
    , elements      :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

data InterfaceElement
    = IterfaceElement
    { name          :: Name
    , comment       :: Maybe Comment
    , elementType   :: ElementType
    } deriving (Eq, Ord, Show)

data ElementType
    = IType
    | IEnum
    | ICommand

data Extension
    = Extension
    { name      :: Name
    , protect   :: Maybe String
    , supported :: Maybe StringGroup
    , comment   :: Maybe Comment
    , requires      :: Set ExtensionElement
    , removes       :: Set ExtensionElement
    } deriving (Eq, Ord, Show)

data ExtensionElement
    = ExtensionElement
    { api           :: Maybe String
    , profileName   :: Maybe ProfileName
    , comment       :: Maybe Comment
    , elements      :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

newtype TypeName
    = TypeName String
    deriving (Eq, Ord, Show)

newtype TypeSuffix
    = TypeSuffix String
    deriving (Eq, Ord, Show)

newtype StringGroup
    = StringGroup String
    deriving (Eq, Ord, Show)

newtype ProfileName
    = ProfileName String
    deriving (Eq, Ord, Show)

newtype Comment
    = Comment String
    deriving (Eq, Ord, Show)

newtype Name
    = Name String
    deriving (Eq, Ord, Show)

newtype ApiReference
    = ApiReference String
    deriving (Eq, Ord, Show)

data Range
    = Range
    { rangeStart    :: Integer
    , rangeEnd      :: Maybe Integer
    }
