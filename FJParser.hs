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

