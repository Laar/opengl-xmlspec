module Text.OpenGL.Types where

import Data.Set (Set)

-- TODO: registry

data Registry
    = Registry
    { regTypes      :: Set Type
    , regEnums      :: Set Enums
    , regCommands   :: Set Command
    , regGroups     :: Set Group
    , regFeatures   :: Set Feature
    , regExtensions :: Set Extension
    }

data Types
    = Types
    { types :: Set Type
    } deriving (Eq, Ord, Show)

data Type
    = Type
    { typeApi       :: Maybe ApiReference
    , typeRequires  :: Maybe String
    , typeName      :: TypeName
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
    deriving (Eq, Ord, Show)

data Groups
    = Groups
    { group :: Set Group
    }
    deriving (Eq, Ord, Show)

data Group
    = Group
    { groupName     :: GroupName
    , groupComment  :: Maybe Comment
    , groupEnums    :: Set EnumName
    } deriving (Eq, Ord, Show)

data Enums
    = Enums
    { enumsNamespace    :: Maybe String
    , enumsGroup        :: Maybe GroupName
    , enumsType         :: Maybe String
    , enumsRange        :: Maybe Range
    , enumsVendor       :: Maybe String
    , enumsComment      :: Maybe Comment
    , enums             :: Set (Either GLEnum Unused)
    } deriving (Eq, Ord, Show)

data GLEnum
    = Enum
    { enumValue     :: Integer
    , enumApi       :: Maybe ApiReference
    , enumType      :: Maybe TypeSuffix
    , enumName      :: EnumName
    , enumAlias     :: Maybe EnumName
    } deriving (Eq, Ord, Show)

data Unused
    = Unused
    { unusedRange   :: Range
    , unusedComment :: Maybe Comment
    } deriving (Eq, Ord, Show)

data Commands
    = Commands
    { commandsNamespace :: Maybe String
    , commands          :: Set Command
    } deriving (Eq, Ord, Show)

data Command
    = Command
    -- Name & ReturnType are defined by the proto element. I doubt the current
    -- return type is permissive enough, but parsing it should give more
    -- insight.
    { commandName       :: CommandName
    , commandReturnType :: ReturnType
    , commandGroup      :: Maybe GroupName
    , commandParams     :: [Param]
    , commandAlias      :: Maybe CommandName
    , commandVecequiv   :: Maybe CommandName
    , commandGlx        :: [GlX]
    } deriving (Eq, Ord, Show)

data ReturnType
    = Void
    | TypeRef TypeName
    deriving (Eq, Ord, Show)

data Param
    = Param
    { -- TODO: interpret & implement
    } deriving (Eq, Ord, Show)

data GlX
    = GlX
    { glXType       :: String
    , glCOpCode     :: Int
    , glXName       :: Maybe Name -- ? Not further defined in the readme => File bug report?
    , glXComment    :: Maybe Comment
    } deriving (Eq, Ord, Show)

data Feature
    = Feature
    { featureApi        :: ApiReference
    , featureName       :: Name
    , featureNumber     :: Float -- TODO: this seems to be a version
    , featureProtect    :: String
    , featureComment    :: Maybe String
    , featureRequires   :: Set FeatureElement
    , featureRemoves    :: Set FeatureElement
    } deriving (Eq, Ord, Show)

data FeatureElement
    = FeatureElement
    { feProfileName   :: Maybe ProfileName
    , feComment       :: Maybe Comment
    , feElements      :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

data InterfaceElement
    = IterfaceElement
    { ieComment     :: Maybe Comment
    , ieElementType :: ElementType
    } deriving (Eq, Ord, Show)

data ElementType
    = IType     TypeName
    | IEnum     EnumName
    | ICommand  CommandName
    deriving (Eq, Ord, Show)

data Extension
    = Extension
    { extensionName         :: Name
    , extensionProtect      :: Maybe String
    , extensionSupported    :: Maybe StringGroup
    , extensionComment      :: Maybe Comment
    , extensionRequires     :: Set ExtensionElement
    , extensionRemoves      :: Set ExtensionElement
    } deriving (Eq, Ord, Show)

data ExtensionElement
    = ExtensionElement
    { eeApi         :: Maybe String
    , eeProfileName :: Maybe ProfileName
    , eeComment     :: Maybe Comment
    , eeElements    :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

newtype TypeName
    = TypeName String
    deriving (Eq, Ord, Show)

newtype EnumName
    = EnumName String
    deriving (Eq, Ord, Show)

newtype CommandName
    = CommandName String
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

newtype GroupName
    = GroupName String
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
    } deriving (Eq, Ord, Show)
