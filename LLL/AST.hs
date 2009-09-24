-------------------------------------------------------------------------------
---- |
---- Module      :  AST
---- Copyright   :  (c) Syoyo Fujita
---- License     :  BSD-style
----
---- Maintainer  :  syoyo@lighttransport.com
---- Stability   :  experimental
---- Portability :  GHC 6.10
----
---- AST         :  Abstract Syntax Tree.
----
-------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module LLL.AST where

import Data.Generics                -- syb

data Type
 = TyUnknown
 | TyVoid
 | TyInt
 | TyFloat
 | TyLong
 | TyDouble
 | TyVector Type Int
 deriving (Show, Eq, Typeable, Data)

data Symbol
  = SymVar  String              -- name of the symbol
  | SymFunc String              -- name of the function
    deriving (Show, Eq, Typeable, Data)

type SymbolTable
  = [(String, [Symbol])]

data Func
  = ShaderFunc Type String      -- FIXME
    deriving (Show, Eq, Typeable, Data)

type LLLUnit = [Func]
