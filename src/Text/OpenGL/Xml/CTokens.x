{
module Text.OpenGL.Xml.CTokens where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+     ;
    struct      { mkTok TStruct     }
    typedef     { mkTok TTypedef    }
    const       { mkTok TConst      }
    signed      { mkTok TSigned     }
    unsigned    { mkTok TUnsigned   }
    \;          { mkTok TSemi       }
    \,          { mkTok TComma      }
    \*          { mkTok TStar       }
    \(          { mkTok TBrackO     }
    \)          { mkTok TBrackC     }
    [$alpha $digit \_]+  {mkStringTok TIdent }

{

scanC :: String -> [CTok]
scanC = alexScanTokens

mkTok :: CToken -> AlexPosn -> String -> CTok
mkTok token pos = const (pos, token)

mkStringTok :: (String -> CToken) -> AlexPosn -> String -> CTok
mkStringTok f pos str = (pos, f str)

type CTok = (AlexPosn, CToken)

data CToken
    = TStruct
    | TTypedef
    | TConst
    | TSigned
    | TUnsigned
    -- Symbols
    | TStar
    | TSemi
    | TComma
    | TBrackO
    | TBrackC
    -- and the indent
    | TIdent String
    deriving (Eq, Ord, Show)
}
