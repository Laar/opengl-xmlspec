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
    { api       :: Maybe String
    , requires  :: Maybe String
    , name      :: Maybe String
    , comment   :: Maybe Comment
    , text1     :: String
    , apiEntry  :: Maybe String
    , text2     :: String
    , name      :: Maybe Name
    , text3     :: String
    } deriving (Eq, Ord, Show)

data Enums
    = Enums
    { namespace :: Maybe String
    , enumClass :: Maybe String
    , enumType  :: Maybe String
    , start     :: Maybe IntegralNum
    , end       :: Maybe IntegralNum
    , vendor    :: Maybe String
    , comment   :: Maybe Comment
    , enums     :: Set (Either Enum Unused)
    } deriving (Eq, Ord, Show)

data Enum
    = Enum
    { value     :: IntegralNum
    , api       :: Maybe String
    , enumType  :: Maybe TypeSuffix
    , name      :: String
    , alias     :: Maybe String
    } deriving (Eq, Ord, Show)

data Unused
    = Unused
    { start     :: IntegralNum
    , end       :: Maybe IntegralNum
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
    { api       :: String
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

-- | Hexadecimal or decimal number (Integer in schema)
data IntegralNum
    = IntegralNum Integer NumType
    deriving (Eq, Ord, Show)

data NumType = Hex | Decicmal
    deriving (Eq, Ord, Show)

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
