-------------------------------------------------------------------------------
---- |
---- Module      :  Parser
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lighttransport.com
---- Stability   :  experimental
---- Portability :  GHC 6.10
----
---- Parser      :  A parser for LLL.
----
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module LLL.Parser where

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Error

import Control.Monad.State
import Debug.Trace

import LLL.AST

-- | LLL parser state
data LLLState = LLLState  { symbolTable :: SymbolTable
                          , n           :: Int
                          }



-- | LLL parser having LLL parser state
type LLLParser a = GenParser Char LLLState a 


-- | Initial state of shader env.
--   Builtin variables are added in global scope.
initLLLState :: LLLState
initLLLState = LLLState {
    symbolTable = [("global", [])]
  , n           = 0
  }
  

-- | Push scope into the symbol table
pushScope :: String -> [Symbol] -> LLLState -> LLLState
pushScope scope xs st =
  st { symbolTable = newTable }

    where
    
      -- Add new scope to the first elem of the list.
      newTable = [(scope, xs)] ++ (symbolTable st)

-- | Pop scope from the symbol table
popScope :: LLLState -> LLLState
popScope st =
  st { symbolTable = newTable }

    where

      -- Pop first scope from the scope chain
      newTable = tail (symbolTable st)

-- | Add the symbol to the first scope in the symbol list.
--   TODO: Check duplication of the symbol.
addSymbol :: Symbol -> LLLState -> LLLState
addSymbol sym st = trace ("// Add " ++ (show sym)) $ 
  st { symbolTable = newTable }

    where

      newTable = case (symbolTable st) of
        [(scope, xs)]     -> [(scope, [sym] ++ xs)]
        ((scope, xs):xxs) -> [(scope, [sym] ++ xs)] ++ xxs

--
-- Topmost parsing rule
--
program :: LLLParser LLLUnit
program               = do  { ast <- many (spaces >> global)
                            ; return ast
                            }
                       <?>   "shader program"


global :: LLLParser Func
global                =   functionDefinition
                      <?> "top level definition"


functionDefinition ::LLLParser Func
functionDefinition    = do  { ty    <- option (TyVoid) funType
                            ; name  <- identifier
                            ; symbol "("
                            ; symbol ")"
                            -- push scope
                            ; updateState (pushScope name [])
                            ; symbol "{"
                            ; symbol "}"
                            -- pop scope
                            ; updateState (popScope)

                            -- Add user function to global scope.
                            ; -- updateState (addSymbol $ mkFunSym name ty (extractTysFromDecls decls))

                            ; return (ShaderFunc ty name)
                            }
                      <?> "function definition"

funType               =     (reserved "void"  >> return TyVoid)


--
-- Parse error reporting routines
--
offt :: Int -> String
offt n = replicate n ' '

showLine :: SourceName -> Int -> Int -> IO ()
showLine name n m =
  do  input <- readFile name

      if (length (lines input)) < n

        then

          if length (lines input) == 0

            then putStrLn ""

          else

            do  { putStrLn $ (lines input) !! ((length (lines input)) - 1)
                ; putStrLn ""
                ; putStrLn $ ((offt (m-1)) ++ "^")
                }

        else

          do  { let l = (lines input) !! (n-1)
              ; putStrLn l
              ; putStrLn $ ((offt (m-1)) ++ "^")
              }



--
-- Parser interface
--
parseLLLFromFile :: LLLParser a -> SourceName -> IO (Either ParseError a)
parseLLLFromFile p fname =
  do { input <- readFile fname
     ; return (runParser p initLLLState fname input)
     }


--
-- Same as in Error.hs of Parsec, just replaced to show error line.
--
showErrorMsg :: ParseError -> FilePath -> String
showErrorMsg err fname =
  show (setSourceName (errorPos err) fname) ++ ":" ++
  showErrorMessages "or" "unknown parse error"
                    "expecting" "unexpected" "end of input"
                   (errorMessages err)

mkMyError :: ParseError -> FilePath -> ParseError
mkMyError err fname = setErrorPos (setSourceName (errorPos err) fname) err


run :: LLLParser LLLUnit -> FilePath -> FilePath -> (LLLUnit -> IO ()) -> IO ()
run p prepname name proc =
  do  { result <- parseLLLFromFile p prepname
      ; case (result) of
          Left err -> do  { -- Parse preprocessed file, but print original file
                            -- when reporting error.
                            putStrLn "Parse err:"
                          ; showLine name (sourceLine (errorPos err)) (sourceColumn (errorPos err))
                          ; print (mkMyError err name)
                          }
          Right x  -> do  { proc x
                          }
      }



runLex :: LLLParser LLLUnit -> FilePath -> FilePath -> (LLLUnit -> IO ()) -> IO ()
runLex p prepname name proc =
  run (do { whiteSpace
          ; x <- p
          ; eof
          ; return x
          }
      ) prepname name proc

--
-- Useful parsing tools
--
lexer           = P.makeTokenParser rslStyle

whiteSpace      = P.whiteSpace lexer
lexeme          = P.lexeme lexer
symbol          = P.symbol lexer
natural         = P.natural lexer
naturalOrFloat  = P.naturalOrFloat lexer
stringLiteral   = P.stringLiteral lexer
float           = P.float lexer
parens          = P.parens lexer
braces          = P.braces lexer
semi            = P.semi lexer
commaSep        = P.commaSep lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer

{-
expr        ::  LLLParser Expr
expr        =   buildExpressionParser table primary
           <?> "expression"

primary     =   try (parens expr)
            <|> triple
            <|> try varRef
            <|> procedureCall   -- Do I really need "try"?
            -- <|> procedureCall   -- Do I really need "try"?
            <|> number
            <|> constString
            <?> "primary"

table       =  [
               -- typecast
                  [typecast]
               -- unary
               ,  [prefix "-" OpSub, prefix "!" OpNeg]

               -- binop
               ,  [binOp "."  OpDot AssocLeft]
               ,  [binOp "*"  OpMul AssocLeft, binOp "/"  OpDiv AssocLeft]
               ,  [binOp "+"  OpAdd AssocLeft, binOp "-"  OpSub AssocLeft]

               -- relop
               ,  [binOp ">"  OpGt  AssocLeft, binOp ">=" OpGe  AssocLeft]
               ,  [binOp "<"  OpLt  AssocLeft, binOp "<=" OpLe  AssocLeft]
               ,  [binOp "==" OpEq  AssocLeft, binOp "!=" OpNeq AssocLeft]

               -- logop
               ,  [binOp "&&" OpAnd AssocLeft, binOp "||" OpOr  AssocLeft]

               -- a ? b : c
               ,  [conditional]

               -- assign
               ,  [ assignOp "="  OpAssign    AssocRight
                  , assignOp "+=" OpAddAssign AssocRight
                  , assignOp "-=" OpSubAssign AssocRight
                  , assignOp "*=" OpMulAssign AssocRight
                  , assignOp "/=" OpDivAssign AssocRight
                  ]
  
               ]

              where

                typecast
                  = Prefix ( do { ty  <- rslType
                                ; spacety <- option "" stringLiteral
                                ; return (\e -> TypeCast Nothing ty spacety e)
                                } <?> "typecast" )

                prefix name f
                  = Prefix ( do { reservedOp name
                                ; return (\x -> UnaryOp Nothing f x)
                                } )

                binOp name f assoc
                  = Infix  ( do { reservedOp name
                                ; return (\x y -> BinOp Nothing f x y)
                                } ) assoc

                conditional
                  = Infix  ( do { reservedOp "?"
                                ; thenExpr <- expr
                                ; reservedOp ":"
                                ; return (\condExpr elseExpr -> Conditional Nothing condExpr thenExpr elseExpr)
                                } <?> "conditional" ) AssocRight

                assignOp name f assoc
                  = Infix  ( do { state <- getState
                                ; reservedOp name
                                ; return (\x y -> mkAssign f x y)
                                } <?> "assign" ) assoc

                -- Make Assign node with flattening expression.
                mkAssign :: Op -> Expr -> Expr -> Expr
                mkAssign op x y = case op of
                  OpAssign    -> Assign Nothing OpAssign x y
                  OpAddAssign -> Assign Nothing OpAssign x (BinOp Nothing OpAdd x y)
                  OpSubAssign -> Assign Nothing OpAssign x (BinOp Nothing OpSub x y)
                  OpMulAssign -> Assign Nothing OpAssign x (BinOp Nothing OpMul x y)
                  OpDivAssign -> Assign Nothing OpAssign x (BinOp Nothing OpDiv x y)
-}

rslStyle = javaStyle
  { reservedNames = [ "const"
                    , "break", "continue"
                    , "while", "if", "for", "solar", "illuminate", "illuminance"
                    , "surface", "volume", "displacement", "imager"
                    , "varying", "uniform", "facevarygin", "facevertex"
                    , "output"
                    , "extern"
                    , "return"
                    , "color", "vector", "normal", "matrix", "point", "void"
                    -- LLL 2.0
                    , "public", "class", "struct"
                    -- More is TODO
                    ]
  , reservedOpNames = ["+", "-", "*", "/"] -- More is TODO
  , caseSensitive   = True
  , commentStart    = "/*"
  , commentEnd      = "*/"
  , commentLine     = "//"
  , nestedComments  = True
  }

