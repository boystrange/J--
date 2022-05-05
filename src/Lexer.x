{
{-# OPTIONS -w  #-}
-- This file is part of FairCheck
--
-- FairCheck is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published
-- by the Free Software Foundation, either version 3 of the License,
-- or (at your option) any later version.
--
-- FairCheck is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with FairCheck. If not, see <http://www.gnu.org/licenses/>.
--
-- Copyright 2021 Luca Padovani

module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit   = 0-9
$alpha   = [A-Za-z]
@next    = $alpha | $digit | \_
@id      = $alpha @next*
@nat     = $digit+
@sign    = [\+\-]
@minus   = \-
@exp     = [eE] @sign? @nat
@int     = @minus? @nat
@float   = (@nat \. @nat?) | (@nat? \. @nat) @exp?
@double  = @float [dD]
@escape  = \\ [\\\'\"abfnrt]
@stringc = @escape | [^\"\\]
@string  = \" @stringc* \"
@charc   = @escape | [^\'\\]
@char    = \' @charc \'

tokens :-
  $white+ ;
  "//".*  ;
  "."     { lex' TokenDOT         }
  ","     { lex' TokenCOMMA       }
  ":"     { lex' TokenCOLON       }
  ";"     { lex' TokenSEMICOLON   }
  "("     { lex' TokenLPAREN      }
  ")"     { lex' TokenRPAREN      }
  "{"     { lex' TokenLBRACE      }
  "}"     { lex' TokenRBRACE      }
  "["     { lex' TokenLBRACK      }
  "]"     { lex' TokenRBRACK      }
  "="     { lex' TokenEQ          }
  "++"    { lex' TokenINC         }
  "--"    { lex' TokenDEC         }
  "+"     { lex' TokenADD         }
  "-"     { lex' TokenSUB         }
  "*"     { lex' TokenMUL         }
  "/"     { lex' TokenDIV         }
  "%"     { lex' TokenMOD         }
  "<"     { lex' TokenLT          }
  "<="    { lex' TokenLE          }
  ">"     { lex' TokenGT          }
  ">="    { lex' TokenGE          }
  "=="    { lex' TokenEQQ         }
  "!="    { lex' TokenNE          }
  "&&"    { lex' TokenAND         }
  "||"    { lex' TokenOR          }
  "?"     { lex' TokenQMARK       }
  "!"     { lex' TokenEMARK       }
  @id     { lex lookupID          }
  @int    { lex TokenINT          }  
  @float  { lex TokenFLOAT        }
  @double { lex (TokenDOUBLE . init) }
  @char   { lex TokenCHAR         }
  @string { lex TokenSTRING       }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

keywords :: [(String, TokenClass)]
keywords = [("new",       TokenNew),
            ("void",      TokenVoid),
            ("boolean",   TokenBoolean),
            ("int",       TokenInt),
            ("float",     TokenFloat),
            ("double",    TokenDouble),
            ("char",      TokenChar),
            ("String",    TokenString),
            ("true",      TokenTrue),
            ("false",     TokenFalse),
            ("if",        TokenIf),
            ("else",      TokenElse),
            ("while",     TokenWhile),
            ("do",        TokenDo),
            ("for",       TokenFor),
            ("length",    TokenLength),
            ("return",    TokenReturn)]

lookupID :: String -> TokenClass
lookupID s = case lookup s keywords of
               Nothing -> TokenID s
               Just tok -> tok

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving (Show)

data TokenClass
  = TokenNew
  | TokenVoid
  | TokenInt
  | TokenBoolean
  | TokenFloat
  | TokenDouble
  | TokenChar
  | TokenString
  | TokenIf
  | TokenElse
  | TokenWhile
  | TokenDo
  | TokenFor
  | TokenLength
  | TokenReturn
  | TokenTrue
  | TokenFalse
  | TokenINT String
  | TokenFLOAT String
  | TokenDOUBLE String
  | TokenCHAR String
  | TokenSTRING String
  | TokenID String
  | TokenEQQ
  | TokenADD
  | TokenSUB
  | TokenMUL
  | TokenDIV
  | TokenMOD
  | TokenINC
  | TokenDEC
  | TokenLT
  | TokenLE
  | TokenGT
  | TokenGE
  | TokenEQ
  | TokenNE
  | TokenAND
  | TokenOR
  | TokenDOT
  | TokenCOMMA
  | TokenCOLON
  | TokenSEMICOLON
  | TokenLPAREN
  | TokenRPAREN
  | TokenLBRACE
  | TokenRBRACE
  | TokenLBRACK
  | TokenRBRACK
  | TokenQMARK
  | TokenEMARK
  | TokenEOF
  deriving (Show)

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
