-- Haskell parser for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 01/2018
---------------------------------------------
module FJParser where
import Data.Map

-- Featherweight Java syntactic constructors
--------------------------------------------
-- class C extends C { C_ f_; K M_ }
data Class = Class String String [(Type,String)] Constr [Method]
           deriving (Show, Eq)

-- C(C_ f_) { super(f_); this.f_.f_; }
data Constr = Constr String [(Type,String)] [String] [(String,String)]
            deriving (Show, Eq)

-- C m(C_ x_) { return e; }
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

-- Auxiliary definitions to Type-Checking
-----------------------------------------
data TypeError = VariableNotFound String
               | FieldNotFound String
               | ClassNotFound String
               | MethodNotFound String String
               | WrongCast String Expr
               | ParamsTypeMismatch [(Expr,Type)]
               | UnknownError Expr
               deriving (Show,Eq)
    
    
-- Function: throwError
-- Objective: Launching a type error.
-- Params: Expected type, Found type, Expression presenting the error.
-- Returns: A type error structure.
----------------------------------------------------------------------
throwError :: TypeError -> Either TypeError Type 
throwError e = Left e

