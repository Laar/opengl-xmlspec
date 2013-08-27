module Text.OpenGL.Types (
    module Text.OpenGL.Types,
    CType(..),
    TypeDef(..),
    BaseType(..),
) where

import Data.Set (Set)

import Text.OpenGL.Xml.TypeTypes

-- TODO: registry

data Registry
    = Registry
    { regTypes      :: Set Type
    , regEnums      :: Set Enums
    , regCommands   :: Set Commands
    , regGroups     :: Set Group
    , regFeatures   :: Set Feature
    , regExtensions :: Set Extension
    } deriving (Eq, Ord, Show)

-- | The description of a type.
data Type
    = Type
    { typeApi       :: Maybe ApiReference
    , typeRequires  :: Maybe TypeName
    -- ^ Requirement of previously defined type(s).
    , typeName      :: TypeName
    -- ^ Name of this type.
    , typeComment   :: Maybe Comment
    , typeImpl      :: TypeImpl
    -- ^ The actual implementation.
    } deriving (Eq, Ord, Show)

-- This probably needs some tweaking...
data TypeImpl
    -- | A type definition
    = TypeDef TypeDef
    -- | Bulk definition of one or more types.
    | BulkDefs  String
    deriving (Eq, Ord, Show)

-- | Grouping of `GLEnums` for restricting parameters or return types of
-- `Commands`. Enums may be part of none or more than one group.
data Group
    = Group
    { groupName     :: GroupName
    , groupComment  :: Maybe Comment
    , groupEnums    :: Set EnumName
    } deriving (Eq, Ord, Show)

-- | An range of enums, unlike group these groupings have more to do with the
-- values of the enums than with their meaning. Though most of the enums in a
-- single range form a logical group.
data Enums
    = Enums
    { enumsNamespace    :: Maybe String
    , enumsGroup        :: Maybe GroupName
    , enumsType         :: Maybe String
    -- ^ Type of an enum, with only 'bitmask' used as value.
    , enumsRange        :: Maybe Range
    , enumsVendor       :: Maybe VendorName
    , enumsComment      :: Maybe Comment
    , enums             :: Set (Either GLEnum Unused)
    } deriving (Eq, Ord, Show)

-- | Describing a single enumeration (or bitmask) value.
data GLEnum
    = Enum
    { enumValue     :: Integer
    , enumApi       :: Maybe ApiReference
    , enumType      :: Maybe TypeSuffix
    -- ^ C-value suffix (u and ull) for correct interpretation of the value.
    , enumName      :: EnumName
    , enumAlias     :: Maybe EnumName
    -- ^ A possibe other enum with the same value. Note, unlike the previous
    -- spec files, the value is included.
    , enumComment   :: Maybe Comment
    } deriving (Eq, Ord, Show)

-- | Marks an unused range of enums.
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
    , commandParams     :: [Param]
    , commandAlias      :: Maybe CommandName
    , commandVecequiv   :: Maybe CommandName
    , commandGlx        :: [GlX]
    } deriving (Eq, Ord, Show)

data ReturnType
    = ReturnType
    { returnType    :: CType
    , returnGroup   :: Maybe GroupName
    } deriving (Eq, Ord, Show)

data Param
    = Param
    { paramType     :: CType
    , paramName     :: String
    , paramGroup    :: Maybe GroupName
    , paramLength   :: Maybe String
    } deriving (Eq, Ord, Show)

data GlX
    = GlX
    { glXType       :: String
    , glCOpCode     :: Int
    , glXName       :: Maybe String -- ? Not further defined in the readme => File bug report?
    , glXComment    :: Maybe Comment
    } deriving (Eq, Ord, Show)

-- | A feature, that is a specific implementation of OpenGL or OpenGL ES. This
-- effectivily generilizes the notion of a version to also include the
-- implementation variant (OpenGL or OpenGL ES).
-- Note that the required and removed feature elements are incremental to a
-- previous version (probably of the same `ApiReference`)
data Feature
    = Feature
    { featureApi        :: ApiReference
    -- ^ The defining api-reference.
    , featureName       :: CPPToken
    -- ^ The name of this feature as a CPP token, e.g. GL_VERSION_3_1.
    , featureNumber     :: Float -- TODO: this seems to be a version
    , featureProtect    :: Maybe CPPToken
    -- ^ Needed other definition as a CPP token.
    , featureComment    :: Maybe Comment
    , featureRequires   :: Set FeatureElement
    , featureRemoves    :: Set FeatureElement
    } deriving (Eq, Ord, Show)

-- | A group of `ElementType`s that are either removed or added.
data FeatureElement
    = FeatureElement
    { feProfileName   :: Maybe ProfileName
    -- ^ The name of the profile this `FeatureElement` is applicable for.
    , feComment       :: Maybe Comment
    , feElements      :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

data InterfaceElement
    = InterfaceElement
    { ieComment     :: Maybe Comment
    , ieElementType :: ElementType
    } deriving (Eq, Ord, Show)

-- | The elements that can be added or removed.
data ElementType
    = IType     TypeName
    | IEnum     EnumName
    | ICommand  CommandName
    deriving (Eq, Ord, Show)

-- | An extension to a (set of) features.
data Extension
    = Extension
    { extensionName         :: CPPToken
    , extensionProtect      :: Maybe String
    , extensionSupported    :: Maybe StringGroup
    -- ^ The `ApiReferences` this extensions works against as regex, on the
    -- underlying string value, with implicit '^' and '&'.
    , extensionComment      :: Maybe Comment
    , extensionRequires     :: Set ExtensionElement
    , extensionRemoves      :: Set ExtensionElement
    -- ^ Note, according to the documentation this SHOULD be empty.
    } deriving (Eq, Ord, Show)

data ExtensionElement
    = ExtensionElement
    { eeApi         :: Maybe ApiReference
    , eeProfileName :: Maybe ProfileName
    , eeComment     :: Maybe Comment
    , eeElements    :: Set InterfaceElement
    } deriving (Eq, Ord, Show)

-- | Name of a `Type`
newtype TypeName
    = TypeName String
    deriving (Eq, Ord, Show)

-- | Name of a `GLEnum`
newtype EnumName
    = EnumName String
    deriving (Eq, Ord, Show)

-- | Name of a `Command`
newtype CommandName
    = CommandName String
    deriving (Eq, Ord, Show)

newtype TypeSuffix
    = TypeSuffix String
    deriving (Eq, Ord, Show)

-- | Version regex, see `extensionSupported`.
newtype StringGroup
    = StringGroup String
    deriving (Eq, Ord, Show)

-- | Name of the opengl profile, e.g. 'compatibility'.
newtype ProfileName
    = ProfileName String
    deriving (Eq, Ord, Show)

-- | Name of a `Group`.
newtype GroupName
    = GroupName String
    deriving (Eq, Ord, Show)

-- | Simple newtype wrapper for comments.
newtype Comment
    = Comment String
    deriving (Eq, Ord, Show)

-- | The CPP preprocessor token for a `Feature` or `Extension`
newtype CPPToken
    = CPPToken String
    deriving (Eq, Ord, Show)

newtype VendorName
    = VendorName String
    deriving (Eq, Ord, Show)

-- | Marker for a definition specific to a certain implementation. According to
-- the documentation to overcome some differences between OpenGL, OpenGL ES 1
-- and OpenGL ES 2/3. (Thus GL-flavour would probably be a more suggestive
-- name).
newtype ApiReference
    = ApiReference String
    deriving (Eq, Ord, Show)

-- | Enumeration value range, posibly without an end.
data Range
    = Range
    { rangeStart    :: Integer
    , rangeEnd      :: Maybe Integer
    } deriving (Eq, Ord, Show)
