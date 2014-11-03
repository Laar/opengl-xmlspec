{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
-----------------------------------------------------------------------------
--
-- | A Parser for the subset of C. The subset is such that the type
-- declarations and function prototypes of the OpenGL spec are parsable.
--
-- The  parse is derived from <http://www.lysator.liu.se/c/ANSI-C-grammar-y.html>
-- . Most of the modifications are the removal and contraction of unused
-- states of the grammar. A major change was needed to get the parsing of
-- identifiers right. More details are at the implementation of the
-- type_specifier rule.
--
-- The resulting parsed C still contains several inconvenient properties,
-- such as that @const int@  and @int const@ are both allowed.
--
-----------------------------------------------------------------------------
module Text.OpenGL.Xml.CParse (
    parseC,
    Declaration,
    DDecl(..),
    DeclS(..),
    Pointer(..),
    TypeS(..), TypeSM(..), TypeQ(..), StorageS(..),
) where

-----------------------------------------------------------------------------

import Control.Applicative hiding (many, (<|>), optional)

import Text.OpenGL.Xml.CTokens
import Text.Parsec hiding (satisfy)

-----------------------------------------------------------------------------

-- | Parses a `String` to a declaration. This declaration may end with a
-- semicolon but need not to.
parseC :: String -> Either ParseError Declaration
parseC = parse (pDeclaration <* eof) "type" . scanC

-----------------------------------------------------------------------------
-- The parsing syntax.

-- | declaration with merged init_declarator(_list).
type Declaration = ([DeclS], Maybe Decl)
-- | declarator
type Decl = (Maybe Pointer, DDecl)

-- | pointer
data Pointer
    = Pointer (Maybe TypeQ) (Maybe Pointer)
    deriving (Eq, Ord, Show)

-- | direct_declarator
data DDecl
    = Ident String
    | DBrack Decl
    | Arr  DDecl String -- constant expression
    | Func DDecl [Declaration] -- restrict to parameters with name
    deriving (Eq, Ord, Show)

-- |  declaration_specifiers.
data DeclS
    = StorageS StorageS
    | TypeSM TypeSM
    | TypeS  TypeS
    | TypeQ  TypeQ
    deriving (Eq, Ord, Show)

-- | Equivalent to type_specifier, but unions and enumerations are removed,
-- in addition for easier processing the modifiers signed and unsigned have
-- their own type `TypeSM`
data TypeS
    = PVoid
    | PChar
    | PShort
    | PInt
    | PInt32T
    | PInt64T
    | PUInt64T
    | PLong
    | PPtrdiffT
    | PSizeT
    | PFloat
    | PDouble
    | PStruct String
    | PIdent  String
    deriving (Eq, Ord, Show)
-- | The signed and unsigned modifiers
data TypeSM
    = PSigned
    | PUnsigned
    deriving (Eq, Ord, Show)

-- | type_qualifier, volatile is left out as it is not used.
data TypeQ
    = PConst
    deriving (Eq, Ord, Show)

-- | storage_class_specifier, only StorageS is relevant.
data StorageS
    = PTypedef
    deriving (Eq, Ord, Show)


-----------------------------------------------------------------------------
-- Declaration parsing

-- | A declaration possibly ended by a semicolon. This allows it to be used
-- to parse the function prototypes and arguments from the spec.
pDeclaration :: P Declaration
pDeclaration = (,) <$> many pDeclS
                   <*> (optionMaybe pDecl)
                   <*  (optional $ tok TSemi)

pDeclS :: P DeclS
pDeclS = choice
    [ TypeS  <$> pTypeS
    , TypeSM <$> pTypeSM
    , TypeQ  <$> pTypeQ
    , StorageS <$> pStorageS
    ]

pDecl :: P Decl
pDecl = (,) <$> optionMaybe pPointer <*> pDDecl1

pPointer :: P Pointer
pPointer = tok TStar *> (Pointer
    <$> optionMaybe pTypeQ
    <*> optionMaybe pPointer)


-- direct_declarator is written in a left recursion that would lead to
-- an infinite recursion if implemented directly. Therefore it is
-- refactored into two separate functions pDDecl1 and pDDecl2

-- part of direct_declarator
pDDecl1 :: P DDecl
pDDecl1 = flip ($) <$> pstart <*> pDDecl2
  where
    pstart :: P DDecl
    pstart = choice
        [ Ident <$> ident
        , DBrack <$> braces pDecl
        ]

pDDecl2 :: P (DDecl -> DDecl)
pDDecl2 = (flip (.) <$> pDDeclPart <*> pDDecl2) <|> pure id
  where
    pDDeclPart = choice
        [ flip Arr <$> rbraces ident
        , flip Func <$> braces pArgs
        ]
    pArgs = pDeclaration `sepBy` tok TComma

-----------------------------------------------------------------------------
-- Type & declaration_specifier parsing

pTypeS :: P TypeS
pTypeS = choice
    [ PVoid      <$ name "void"
    , PChar      <$ name "char"
    , PShort     <$ name "short"
    , PInt       <$ name "int"
    , PInt32T    <$ name "int32_t"
    , PInt64T    <$ name "int64_t"
    , PUInt64T   <$ name "uint64_t"
    , PLong      <$ name "long"
    , PPtrdiffT  <$ name "ptrdiff_t"
    , PSizeT     <$ name "size_t"
    , PFloat     <$ name "float"
    , PDouble    <$ name "double"
    , PStruct    <$> (tok TStruct *> ident)
    , PIdent     <$> try (ident <* pNotEnd) -- see Note [Type Identifiers]
    ] <?> "[Type specifier]"
  where
    pNotEnd = lookAhead $ () <$ satisfy' (\t -> not $ t `elem`
        -- see Note [Type Identifiers]
        [ TSemi     -- for x in @typedef int x;@
        , TBrackO   -- for x in @int x();@
        , TBrackC   -- for x in @int f(int x)@
        , TComma    -- for x in @int f(int x, int b)@
        , TRBrackO  -- for x in @int f(int x[])@
        ])

{- Note [Type Identifiers]
The C grammer contains the rule
    type_specifier
        : void
        ...
        | TYPE_NAME
        ...
Thus an identifier should only be matched if it is a name of a type. But as
we don't keep a set of the defined types, it is not easy to implement this
rule. The naive implementation, without any check would fail on for example
    typedef int glint;
where glint will not be parsed as part the identifier a `DDecl` but as
`TypeS`. Removing the optionality of `Decl` (and thus `DDecl`) in
`Declaration` would seem a solution, but it is needed for constructs like
    struct _cl_event;
Furthermore note that in function arguments it is allowed to have
    struct _cl_event event
which makes it impossible to get this to work be removing the identifier from
`PStruct`. Therefore an alternative solution is made to prevent TypeS to also
consume the relevant identifier. If the specifier is followed by one of the
listed tokens it is not considered a TYPE_NAME.
-}

pTypeSM :: P TypeSM
pTypeSM = choice
    [ PSigned   <$ tok TSigned
    , PUnsigned <$ tok TUnsigned
    ]

pTypeQ :: P TypeQ
pTypeQ = PConst <$ tok TConst


pStorageS :: P StorageS
pStorageS = PTypedef <$ tok TTypedef

-----------------------------------------------------------------------------
-- Internals and utilities needed for the parsing.

type P = Parsec [CTok] ()

braces :: (Stream [CTok] m CTok) => ParsecT [CTok] u m a -> ParsecT [CTok] u m a
braces = between (tok TBrackO) (tok TBrackC)

rbraces :: (Stream [CTok] m CTok) => ParsecT [CTok] u m a -> ParsecT [CTok] u m a
rbraces = between (tok TRBrackO) (tok TRBrackC)

-- | This parser succeeds whenever the given predicate returns true when called with
-- parsed `CTok`. Same as 'Text.Parsec.Char.satisfy'.
satisfy :: (Stream [CTok] m CTok) => (CTok -> Bool) -> ParsecT [CTok] u m CToken
satisfy f = tokenPrim show nextPos tokeq
    where
        nextPos :: SourcePos -> CTok -> [CTok] -> SourcePos
        nextPos pos _ (((AlexPn _ l c),_):_) = setSourceColumn (setSourceLine pos l) c
        nextPos pos _ [] = pos

        tokeq :: CTok -> Maybe CToken
        tokeq t = if f t then Just (snd t) else Nothing

satisfy' :: (Stream [CTok] m CTok) => (CToken -> Bool) -> ParsecT [CTok] u m CToken
satisfy' f = satisfy (f . snd)

-- | Parses given `CToken`.
tok :: (Stream [CTok] m CTok) => CToken -> ParsecT [CTok] u m CToken
tok t = satisfy' ((==) t) <?> show t

ident :: (Stream [CTok] m CTok) => ParsecT [CTok] u m String
ident = (extract <$> satisfy' isIdent ) <?> "identifier"
  where
    isIdent (TIdent _) = True
    isIdent _          = False
    extract (TIdent c) = c
    extract _          = error "Not an identifier"

name :: (Stream [CTok] m CTok) => String -> ParsecT [CTok] u m ()
name n = () <$ satisfy' matches <?> n
  where
    matches (TIdent n') = n' == n
    matches _           = False

-----------------------------------------------------------------------------
