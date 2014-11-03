{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
-- | The parsed C from "Text.OpenGL.Xml.CParse" needs to be interpreted to
-- get to the more restricted and useful types of
-- "Text.OpenGL.Xml.TypeTypes".
--
-- This transformation is done in this module and in specific by
-- `parseStringTypeDef` and `parseStringFuncPart`.
-----------------------------------------------------------------------------
module Text.OpenGL.Xml.CProcess (
    parseStringTypeDef,
    parseStringFuncPart,
) where

-----------------------------------------------------------------------------

import Control.Applicative
import Control.Monad
import Text.Parsec ()

import Text.OpenGL.Xml.CParse
import Text.OpenGL.Xml.TypeTypes

-----------------------------------------------------------------------------

-- | Parses and transforms a `String` into a `TypeDef`. For this it requires
-- that it contains the typedef keyword or that it is a StructDef. Examples
-- of valid input are:
--      typedef int glint;
--      struct _cl_event;
--      typedef void (* f)(int x[2]);
parseStringTypeDef :: String -> Either String TypeDef
parseStringTypeDef = pTypeDef

-- | Parses and transforms a `String` into a CType. This is useful for the
-- function prototypes and arguments from the specification. The string
-- should contain a type and the name of the argument or function. Examples
-- of valid input are:
--      int x
--      struct _cl_event event
--      const void *f
parseStringFuncPart :: String -> Either String CType
parseStringFuncPart = pCType

-- Helper function to get rid of the ParseError type.
parseC' :: String -> Either String Declaration
parseC' s = case parseC s of
    Left  e -> Left $ show e
    Right d -> Right d

-----------------------------------------------------------------------------

pTypeDef :: String -> Either String TypeDef
pTypeDef s = do
    d  <- parseC' s
    pAlias d <|> pFuncDef d <|> pStructDef d
        <|> Left ("Typedef parsing failed for:" ++ s)

pCType :: String -> Either String CType
pCType s = do
    d <- parseC' s
    fst <$> pCType' False d
-- | Parses type aliases, e.g. @typedef int glint@.
pAlias :: Declaration -> Either String TypeDef
pAlias decl  = Alias . fst <$>  pCType' True decl

-- | Parses function pointer definitions, e.g. @void (*f)(int x)@
pFuncDef :: Declaration -> Either String TypeDef
pFuncDef (d, Just (Nothing, Func
        (DBrack (Just (Pointer Nothing Nothing), Ident _))
        as
    )) = do
    pt <- declSToPartialType d
    ct <- partialtypeToCType True pt
    as' <- mapM (pCType' False) as
    return $ FuncDef ct as'
pFuncDef _ =  Left "Invalid function declaration"

-- | Parses Struct definitions, e.g. @struct _cl_event@.
pStructDef :: Declaration -> Either String TypeDef
pStructDef ([TypeS (PStruct n)], Nothing) = Right $ StructDef n
pStructDef _ = Left $ "Invalid struct def"

-----------------------------------------------------------------------------

-- | Extracts from a Declaration the relevant `CType` allong with the name of
-- the identifier.
pCType' :: IsTypedef -> Declaration -> Either String (CType, String)
pCType' _    (_, Nothing) = Left "No identifier for pCType."
pCType' isTD (ds, Just (p, dd)) = do
    pt <- declSToPartialType ds
    ct <- partialtypeToCType isTD pt
    let ct' = mPointerToCType p ct
    pDDecl dd ct'
 where
    pDDecl :: DDecl -> CType -> Either String (CType, String)
    pDDecl d ct = case d of
        Ident n   -> return (ct, n)
        Arr d' l  -> do
            (ct', n) <- pDDecl d' ct
            return (ArrayOf ct' l, n)
        _ -> Left "TODO: not currently needed"

type IsTypedef = Bool

mPointerToCType :: Maybe Pointer -> (CType -> CType)
mPointerToCType = maybe id $ \p -> case p of
    Pointer Nothing       mp -> mPointerToCType mp .          Ptr
    Pointer (Just PConst) mp -> mPointerToCType mp . CConst . Ptr

-----------------------------------------------------------------------------

-- | Converts the list with `DeclS` to the more convinient `PartialType`
-- which contains the same information but more condensed.
declSToPartialType :: [DeclS] -> Either String PartialType
declSToPartialType = foldM (flip addDeclS) startType

-- | Converts the `PartialType` to the `CType` that it corresponds to.
partialtypeToCType :: IsTypedef -> PartialType -> Either String CType
partialtypeToCType td pt
    | not (isTypedef pt) && td = Left $ "Expected typedef, but received none."
    | isTypedef pt   && not td = Left $ "Unexpected typedef."
    | signError                = Left $
            "Unallowed sign for a type " ++ show (typeS pt)
    | otherwise = case typeS pt of
        Nothing -> Left "No type"
        Just (PStruct n) -> Right . addConstT $ Struct n
        Just (PIdent n) -> Right . addConstT $ AliasType n
        Just t -> Right . addConstT . BaseType . addSign $ mapBaseType t
  where
    isSignAllowed = case typeS pt of
        Nothing -> True -- an absent type is a bigger problem,
                        -- so report that first.
        Just t  -> t `elem` [PChar, PShort, PInt, PInt32T, PInt64T, PLong]
    signError = maybe False (not . const isSignAllowed) $ isSigned pt
    addConstT  :: CType -> CType
    addConstT  = if isConst pt then CConst else id
    addSign :: BaseType -> BaseType
    addSign = case isSigned pt of
        Nothing    -> id
        Just True  -> Signed
        Just False -> Unsigned
    mapBaseType :: TypeS -> BaseType
    mapBaseType t = case t of
        PVoid       -> Void
        PChar       -> Char
        PShort      -> Short
        PInt        -> Int
        PInt32T     -> Int32T
        PInt64T     -> Int64T
        PUInt64T    -> UInt64T
        PLong       -> Long
        PPtrdiffT   -> PtrdiffT
        PSizeT      -> SizeT
        PFloat      -> Float
        PDouble     -> Double
        _           -> error $
            "partialtypeToCType.mapBaseType: unexpected type " ++ show t

-- | A condensed version of all the `DeclS` in front of a declaration. This
-- is needed as the order does not matter for the `DeclS`. For example
-- @const int@ and @int const@ are the same. Combine this with the typedef
-- keyword and possibly signedness to get a lot of posible orders.
data PartialType
    = PartialType
    { isConst    :: Bool
    , isTypedef  :: Bool
    , isSigned   :: Maybe Bool
    , typeS      :: Maybe TypeS
    } deriving (Eq, Ord, Show)

-- The starting point, no information given.
startType :: PartialType
startType = PartialType False False Nothing Nothing

addDeclS :: DeclS -> PartialType -> Either String PartialType
addDeclS d = case d of
    StorageS PTypedef   -> addTypedef
    TypeS ts            -> addTypeS ts
    TypeSM PSigned      -> addSigned True
    TypeSM PUnsigned    -> addSigned False
    TypeQ  PConst       -> addConst

addConst :: PartialType -> Either String PartialType
addConst pt = Right pt{isConst = True} -- allow multiple const-s.

addTypedef :: PartialType -> Either String PartialType
addTypedef pt =
    if isTypedef pt
     then Left "Multiple typedefs" -- explicitly forbidden in C
     else Right pt{isTypedef = True}

addSigned :: Bool -> PartialType -> Either String PartialType
addSigned sn pt = case isSigned pt of
    Nothing -> Right pt{isSigned = Just sn}
    Just s  | s == sn   -> Right pt
            | otherwise -> Left "Mixed use of singed and unsigned"

addTypeS :: TypeS -> PartialType -> Either String PartialType
addTypeS t pt = case typeS pt of
    Nothing -> Right pt{typeS = Just t}
    Just t' -> Left $ "Multiple types, both " ++ show t ++ " and " ++ show t'

-----------------------------------------------------------------------------
