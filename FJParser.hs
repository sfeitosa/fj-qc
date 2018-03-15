-- Haskell parser for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 01/2018
---------------------------------------------
module FJParser where
import Data.Map

-- Featherweight Java syntactic constructors
--------------------------------------------
data Class = Class String String [(Type,String)] Constr [Method]
           deriving (Show, Eq)

data Constr = Constr String [(Type,String)] [String] [(String,String)]
            deriving (Show, Eq)

data Method = Method Type String [(Type,String)] Expr
            deriving (Show, Eq)

data Expr = Var String                               -- Variable
          | FieldAccess Expr String                  -- Field Access
          | MethodInvk Expr String [Expr]            -- Method Invocation
          | CreateObject String [Expr]               -- Object Instantiation
          | Cast String Expr                         -- Cast
          deriving (Show, Eq)

-- Featherweight Java nominal typing
------------------------------------
data Type = TypeClass String
          deriving (Show, Eq)

-- Featherweight Java auxiliary definitions
-------------------------------------------
type Env = Map String Type
type CT = Map String Class

