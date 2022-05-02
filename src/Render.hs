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

-- |Pretty printer for session types and error messages.
module Render
  ( printTitle
  , printWarning
  , printOK
  , printNO
  , printType )
where

import Atoms
import Type
import Language
import SourceLanguage
import Prelude hiding ((<>))
import Prettyprinter
import qualified Prettyprinter.Render.String as PR
import qualified Prettyprinter.Render.Terminal as PT
import qualified Data.Map as Map
import qualified Data.Set as S
import qualified Data.List as List

-- PRETTY PRINTER COMPATIBILITY

type Document = Doc PT.AnsiStyle

keyword :: String -> Document
keyword = annotate (PT.color PT.Blue) . pretty

identifier :: String -> Document
identifier = pretty

constant :: String -> Document
constant = annotate (PT.color PT.Magenta) . pretty

operator :: String -> Document
operator = annotate PT.bold . pretty

emark :: Document
emark = operator "!"

qmark :: Document
qmark = operator "?"

dot :: Document
dot = operator "."

bar :: Document
bar = operator "|"

ampersand :: Document
ampersand = operator "&"

-- UTILITIES

embrace :: Document -> Document -> Document -> [Document] -> Document
embrace open close sep ds = align (encloseSep (open <> space) (space <> close) (sep <> space) ds)

sepembrace :: Document -> Document -> Document -> [Document] -> Document
sepembrace open close sep ds = embrace open close sep (map (<> space) (init ds) ++ [last ds])

-- TYPES

prettyType :: Type -> Document
prettyType = annotate (PT.colorDull PT.Cyan) . undefined

instance Show DataType where
  show BooleanType = "boolean"
  show IntType = "int"
  show FloatType = "float"
  show DoubleType = "double"
  show CharType = "char"
  show StringType = "String"

instance Show Type where
  show VoidType = "void"
  show (DataType dt) = show dt
  show (ArrayType t) = show t ++ "[]"
  show (MethodType rt ts) = "<method>"

instance Show Reference where
  show (IdRef x) = showWithPos x
  show (ArrayRef ref _) = show ref ++ "[...]"

instance Show UnOp where
  show NEG = "-"
  show POS = "+"

instance Show BinOp where
  show ADD = "+"
  show SUB = "-"
  show MUL = "*"
  show DIV = "/"
  show MOD = "%"

instance Show RelOp where
  show JLT = "<"
  show JLE = "<="
  show JGT = ">"
  show JGE = ">="
  show JEQ = "=="
  show JNE = "!="

instance Show IncDecOp where
  show PREINC = "++"
  show PREDEC = "--"
  show POSTINC = "++"
  show POSTDEC = "--"

instance Show Label where
    show (L n) = "L" ++ show n

-- |Print a type.
printType :: Type -> IO ()
printType = PT.putDoc . prettyType

-- AUXILIARY PRINTING OPERATIONS

-- |Print a newline.
printNewLine :: IO ()
printNewLine = putStrLn ""

-- |Print a string with style annotations.
printAnnotatedString :: [PT.AnsiStyle] -> String -> IO ()
printAnnotatedString anns msg = PT.putDoc (foldr annotate (pretty msg) anns)

-- |Print a string as a title.
printTitle :: String -> IO ()
printTitle msg = printAnnotatedString [PT.bold, PT.underlined] msg >> printNewLine

-- |Print a warning message.
printWarning :: String -> IO ()
printWarning msg = printAnnotatedString [PT.color PT.Red] msg >> printNewLine

-- |Print an error message.
printNO :: String -> IO ()
printNO msg = do
  printAnnotatedString [PT.color PT.Red] "NO:"
  putStrLn $ " " ++ msg

-- |Print a success message.
printOK :: Maybe String -> IO ()
printOK msg = do
  printAnnotatedString [PT.bold, PT.color PT.Green] "OK"
  case msg of
    Nothing -> printNewLine
    Just m -> putStrLn $ " (" ++ m ++ ")"
