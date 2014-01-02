{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction #-}
module Text.OpenGL.Xml.CParse (
    parseStringTypeDef,
    parseStringFuncPart,
    TypeDef(..),
    BaseType(..),
    CType(..),
) where

import Control.Applicative hiding (many)
import Data.Monoid

import Text.OpenGL.Xml.CTokens
import Text.OpenGL.Xml.TypeTypes
import Text.Parsec hiding (satisfy)

type P = Parsec [CTok] ()

parseStringTypeDef :: String -> Either ParseError TypeDef
parseStringTypeDef
    = parse (pTypeDef <* eof) "type" . scanC

parseStringFuncPart :: String -> Either ParseError CType
parseStringFuncPart
    = parse (fst <$> pArg <* eof) "param | proto" . scanC

pTypeDef :: P TypeDef
pTypeDef = choice
    [ try $ StructDef   <$> (tok TStruct  *> ident)
    , try $ Alias       <$> (tok TTypedef *> pType <* ident)
    , try $ FuncDef     <$> (tok TTypedef *> pType)
                            <* braces (tok TStar <* ident)
                        <*> braces pArgs
    ] <* tok TSemi
  where
    pArgs :: P [(CType, String)]
    pArgs = pArg `sepBy` tok TComma

pArg :: P (CType, String)
pArg = (,) <$> pType <*> ident

pType :: P CType
pType = flip ($)
    <$> pTypeBase
    <*> (appEndo . mconcat . map Endo <$> many (try pPointer))

pPointer :: P (CType -> CType)
pPointer = try $ (.)
    <$> try (Ptr <$ tok TStar)
    <*> try (option id (CConst <$ tok TConst))

pTypeBase :: P CType
pTypeBase = choice
    [ try $ Struct    <$> (tok TStruct *> ident)
    , try $ CConst    <$> (tok TConst  *> pTypeBase)
    , try $ BaseType  <$> pBaseType
    , try $ AliasType <$> ident
    ]

pBaseType :: P BaseType
pBaseType = choice
    [ Void      <$ name "void"
    , Char      <$ name "char"
    , Short     <$ name "short"
    , Int       <$ name "int"
    , Int32T    <$ name "int32_t"
    , Int64T    <$ name "int64_t"
    , UInt64T   <$ name "uint64_t"
    , Long      <$ name "long"
    , PtrdiffT  <$ name "ptrdiff_t"
    , SizeT     <$ name "size_t"
    , Float     <$ name "float"
    , Double    <$ name "double"
    , Signed    <$> try (tok TSigned   *> pBaseType)
    , Unsigned  <$> try (tok TUnsigned *> pBaseType)
    ] <?> "[basic type]"

braces :: (Stream [CTok] m CTok) => ParsecT [CTok] u m a -> ParsecT [CTok] u m a
braces = between (tok TBrackO) (tok TBrackC)

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
