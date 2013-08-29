module Text.OpenGL.Names (
    decomposeExtensionToken,
    decomposeEnumName,
    decomposeCommandName
) where

import Data.Char
import Data.List.Split
import Text.OpenGL.Types

decomposeExtensionToken :: CPPToken -> Maybe (VendorName, String)
decomposeExtensionToken (CPPToken s) = case s of
    'G':'L':'_':rs -> case break (== '_') rs of
        ([],_)      -> Nothing
        (_,[])      -> Nothing
        (v,'_':n)   -> Just (VendorName v, n)
        _           -> Nothing
    _ -> Nothing

decomposeEnumName :: EnumName -> Maybe [String]
decomposeEnumName (EnumName n) = case n of
    'G':'L':'_':rs | not $ null rs -> Just $ splitWhen (=='_') rs
    _   -> Nothing

decomposeCommandName :: CommandName -> Maybe [String]
decomposeCommandName (CommandName n) = case n of
    'g':'l':rs | not $ null rs -> Just $ split splitter rs
        where splitter = dropInitBlank $ keepDelimsL (whenElt isUpper)
    _   -> Nothing
