{
{-# OPTIONS -w #-}
-- This file is part of J--

-- J-- is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- J-- is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with J--. If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2022 Luca Padovani

-- |This module implements the parser for FairCheck scripts.
module Parser (parseProgram) where

import Lexer
import Atoms
import Type
import Language

import Data.Either (partitionEithers)
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  ID        { $$@(Token _ (TokenID _)) }
  INT       { $$@(Token _ (TokenINT _)) }
  FLOAT     { $$@(Token _ (TokenFLOAT _)) }
  DOUBLE    { $$@(Token _ (TokenDOUBLE _)) }
  CHAR      { $$@(Token _ (TokenCHAR _)) }
  STRING    { $$@(Token _ (TokenSTRING _)) }
  VOIDKW    { Token _ TokenVoid }
  BOOLEANKW { Token _ TokenBoolean }
  INTKW     { Token _ TokenInt }
  FLOATKW   { Token _ TokenFloat }
  DOUBLEKW  { Token _ TokenDouble }
  CHARKW    { Token _ TokenChar }
  STRINGKW  { Token _ TokenString }
  IFKW      { Token _ TokenIf }
  ELSEKW    { Token _ TokenElse }
  WHILEKW   { Token _ TokenWhile }
  DOKW      { Token _ TokenDo }
  FORKW     { Token _ TokenFor }
  NEWKW     { Token _ TokenNew }
  TRUEKW    { Token _ TokenTrue }
  FALSEKW   { Token _ TokenFalse }
  RETURNKW  { Token _ TokenReturn }
  '='       { Token _ TokenEQ }
  '=='      { Token _ TokenEQQ }
  '!='      { Token _ TokenNE }
  '&&'      { Token _ TokenAND }
  '||'      { Token _ TokenOR }
  '++'      { Token _ TokenINC }
  '--'      { Token _ TokenDEC }
  '<'       { Token _ TokenLT }
  '>'       { Token _ TokenGT }
  '<='      { Token _ TokenLE }
  '>='      { Token _ TokenGE }
  '.'       { Token _ TokenDOT }
  ':'       { Token _ TokenCOLON }
  ';'       { Token _ TokenSEMICOLON }
  ','       { Token _ TokenCOMMA }
  '('       { Token _ TokenLPAREN }
  ')'       { Token _ TokenRPAREN }
  '{'       { Token _ TokenLBRACE }
  '}'       { Token _ TokenRBRACE }
  '['       { Token _ TokenLBRACK }
  ']'       { Token _ TokenRBRACK }
  '+'       { Token _ TokenADD }
  '-'       { Token _ TokenSUB }
  '*'       { Token _ TokenMUL }
  '/'       { Token _ TokenDIV }
  '%'       { Token _ TokenMOD }
  '!'       { Token _ TokenEMARK }

%right '='
%left '||' '&&'
%nonassoc '<' '>' '<=' '>=' '==' '!='
%left '+' '-'
%left '*' '/' '%'
%nonassoc '!' UNARY

%%

-- PROGRAMS

Program
  : ElementList { partitionEithers $1 }

-- ELEMENTS

ElementList
  : { [] }
  | Element ElementList { $1 : $2 }

Element
  : Method { Left $1 }
  | Statement { Right $1 }

-- METHODS

Method
  : Type Id '(' ArgList ')' Statement { Method $1 $2 $4 $6 }

ArgList
  : { [] }
  | ArgNeList { $1 }

ArgNeList
  : Arg { [$1] }
  | Arg ',' ArgNeList { $1 : $3 }

Arg
  : Type Id { ($2, $1) }

-- TYPES

Type
  : VOIDKW { VoidType }
  | DataType { DataType $1 }
  | Type '[' ']' { ArrayType $1 }

DataType
  : BOOLEANKW { BooleanType }
  | INTKW     { IntType }
  | FLOATKW   { FloatType }
  | DOUBLEKW  { DoubleType }
  | CHARKW    { CharType }
  | STRINGKW  { StringType }

-- STATEMENTS

StatementList
  : { Empty }
  | Statement StatementList { Seq $1 $2 }

Statement
  : IFKW '(' Expression ')' Statement ElseOpt { If $3 $5 $6 }
  | WHILEKW '(' Expression ')' Statement { While $3 $5 }
  | DOKW Statement WHILEKW '(' Expression ')' { Do $2 $5 }
  | FORKW '(' StatementOpt ';' TestOpt ';' StatementOpt ')' Statement { expandFor $3 $5 $7 $9 }
  | RETURNKW ExpressionOpt ';' { Return $2 }
  | Expression ';' { Expression $1 }
  | Type InitNeList ';' { expandLocals $1 $2 }
  | '{' StatementList '}' { Block $2 }

StatementOpt
  : { Empty }
  | Statement { $1 }

ElseOpt
  : { Empty }
  | ELSEKW Statement { $2 }

InitNeList
  : Init { [$1] }
  | Init ',' InitNeList { $1 : $3 }

Init
  : Id { ($1, Nothing) }
  | Id '=' Expression { ($1, Just $3) }

-- REFERENCES

Ref
  : Id { IdRef $1 }
  | Ref '[' Expression ']' { ArrayRef $1 $3 }

-- EXPRESSIONS

TestOpt
  : { Literal (Boolean True) }
  | Expression { $1 }

ExpressionOpt
  : { Nothing }
  | Expression { Just $1 }

ExpressionList
  : { [] }
  | ExpressionNeList { $1 }

ExpressionNeList
  : Expression { [$1] }
  | Expression ',' ExpressionNeList { $1 : $3 }

Expression
  : Literal { Literal $1 }
  | Id '(' ExpressionList ')' { Call $1 $3 }
  | NEWKW Type '[' Expression ']' { New $2 $4 }
  | Ref { Ref $1 }
  | '(' Expression ')' { $2 }
  | Ref '=' Expression { Assign $1 $3 }
  | Expression '+' Expression { Binary ADD $1 $3 }
  | Expression '-' Expression { Binary SUB $1 $3 }
  | Expression '*' Expression { Binary MUL $1 $3 }
  | Expression '/' Expression { Binary DIV $1 $3 }
  | Expression '%' Expression { Binary MOD $1 $3 }
  | Expression '&&' Expression { Binary AND $1 $3 }
  | Expression '||' Expression { Binary OR $1 $3 }
  | Expression '<' Expression { Binary JLT $1 $3 }
  | Expression '>' Expression { Binary JGT $3 $1 }
  | Expression '<=' Expression { Binary JLE $1 $3 }
  | Expression '>=' Expression { Binary JGE $1 $3 }
  | Expression '==' Expression { Binary JEQ $1 $3 }
  | Expression '!=' Expression { Binary JNE $1 $3 }
  | '(' Type ')' Expression %prec UNARY { Cast $2 $4 }
  | Ref '++' { IncDec POSTINC $1 }
  | Ref '--' { IncDec POSTDEC $1 }
  | '++' Ref { IncDec PREINC $2 }
  | '--' Ref { IncDec PREDEC $2 }
  | '-' Expression %prec UNARY { Unary NEG $2 }
  | '+' Expression %prec UNARY { Unary POS $2 }
  | '!' Expression { Unary NOT $2 }

Literal
  : INT { Int (read (getId $1)) }
  | FLOAT { Float (read (getId $1)) }
  | DOUBLE { Double (read (getId $1)) }
  | Boolean { Boolean $1 }
  | CHAR { Char $ (getId $1)!!0 }
  | STRING { String (getId $1) }

Boolean
  : TRUEKW { True }
  | FALSEKW { False }

-- -- IDENTIFIERS

Id
  : ID { Id (At $ getPos $1) (getId $1) }

{
getId :: Token -> String
getId (Token _ (TokenID x)) = x
getId (Token _ (TokenINT x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

expandLocals :: Type -> [(Id, Maybe SourceExpression)] -> SourceStatement
expandLocals t = foldl Seq Empty . map aux
  where
    aux (x, Nothing) = Local t x
    aux (x, Just expr) = Seq (Local t x) (Expression $ Assign (IdRef x) expr)

expandFor :: SourceStatement -> SourceExpression -> SourceStatement -> SourceStatement -> SourceStatement
expandFor init expr incr body = Block $ Seq init $ While expr $ Seq body incr

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProgram :: FilePath -> String -> Either String ([SourceMethod], [SourceStatement])
parseProgram = runAlex' parse
}
