{-# LANGUAGE CPP, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Language.Python.Common.Token
-- Copyright   : (c) 2009 Bernie Pope
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical tokens for the Python lexer. Contains the superset of tokens from
-- version 2 and version 3 of Python (they are mostly the same).
-----------------------------------------------------------------------------

module Language.JavaScript.Parser.Token
    (
      -- * The tokens
      Token (..)
    , CommentAnnotation (..)
    -- * String conversion
    , debugTokenString
    -- * Classification
    -- TokenClass (..),
    ) where

import Data.Data
import Language.JavaScript.Parser.SrcLocation

data CommentAnnotation
    = CommentA TokenPosn String
    | WhiteSpace TokenPosn String
    | NoComment
    deriving (Eq, Show, Typeable, Data, Read)

-- | Lexical tokens.
-- Each may be annotated with any comment occuring between the prior token and this one
data Token
    -- Comment
    = CommentToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation] } -- ^ Single line comment.
    | WsToken      { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation] } -- ^ White space, for preservation.

    -- Identifiers
    | IdentifierToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }            -- ^ Identifier.

    -- Javascript Literals

    | DecimalToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Decimal
    | HexIntegerToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Hexadecimal Integer
    | OctalToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Octal Integer
    | StringToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, token_delimiter :: !Char, tokenComment :: ![CommentAnnotation]  }
    -- ^ Literal: string, delimited by either single or double quotes
    | RegExToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]   }
    -- ^ Literal: Regular Expression

    -- Keywords
    | BreakToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | CaseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | CatchToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ConstToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ContinueToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | DebuggerToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | DefaultToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | DeleteToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | DoToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ElseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | EnumToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | FalseToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | FinallyToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ForToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | FunctionToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | IfToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | InToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | InstanceofToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | NewToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | NullToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ReturnToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | SwitchToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ThisToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | ThrowToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | TrueToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | TryToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | TypeofToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | VarToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | VoidToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | WhileToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | WithToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    -- Future reserved words
    | FutureToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    -- Needed, not sure what they are though.
    | GetToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | SetToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }

    -- Delimiters
    -- Operators
    | AutoSemiToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | SemiColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CommaToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | HookToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ColonToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | OrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseOrToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseXorToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseAndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictEqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | EqToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | AssignToken { tokenSpan :: !TokenPosn, tokenLiteral :: !String, tokenComment :: ![CommentAnnotation]  }
    | SimpleAssignToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | StrictNeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | UrshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RshToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GeToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | GtToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | IncrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DecrementToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | PlusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MinusToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | MulToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DivToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | ModToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | NotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | BitwiseNotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | DotToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightBracketToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightCurlyToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | LeftParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | RightParenToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }
    | CondcommentEndToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }

    -- Special cases
    | TailToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  } -- ^ Stuff between last JS and EOF
    | EOFToken { tokenSpan :: !TokenPosn, tokenComment :: ![CommentAnnotation]  }  -- ^ End of file
    deriving (Eq, Show, Typeable)


-- | Produce a string from a token containing detailed information. Mainly intended for debugging.
debugTokenString :: Token -> String
debugTokenString = takeWhile (/= ' ') . show
