-- QuickCheck property-based testing for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 03/2018
-----------------------------------------------------------
module Tests where
import FJParser
import FJUtils
import FJTypeChecker
import CT
import Test.QuickCheck
import Data.Map
import Data.List
import Data.Maybe
import Control.Monad

-- Function: genVar
-------------------
genVar :: Gen Char
genVar = elements ['a' .. 'z']

-- Function: maybeElements
-- Objective: Get one element of a given list.
-- Params: List of a generic value.
-- Returns: A monad maybe containing the element or Nothing.
------------------------------------------------------------
maybeElements :: [a] -> Maybe (Gen a)
maybeElements [] = Nothing
maybeElements xs = Just (elements xs)

-- Function: pickVar
-- Objective: Pick a variable of a given type from context.
-- Params: Context, Class name.
-- Returns: A variable of a given type.
-----------------------------------------------------------
pickVar :: Env -> Type -> Maybe (Gen Expr)
pickVar ctx t = maybeElements [Var x | (x, t') <- Data.Map.assocs ctx, t' == t]

-- Function: genFieldAccess
-- Objective: Generate an expression using FieldAccess constructor.
-- Params: Size, Context, Class table, Class name, Field
-- Returns: A FieldAccess expression.
-------------------------------------------------------------------
genFieldAccess :: Int -> Env -> CT -> String -> String -> Gen Expr
genFieldAccess size ctx ct t f = do e <- genExpr (size - 1) ct ctx t
                                    return (FieldAccess e f)

-- Function: genMethodInvk
-- Objective: Generate an expression using MethodInvk constructor.
-- Params: Size, Context, Class table, Class name, Method name, Method params.
-- Returns: A MethodInvk expression.
------------------------------------------------------------------------------
genMethodInvk :: Int -> Env -> CT -> String -> String -> [String] -> Gen Expr
genMethodInvk size ctx ct t m pt = 
  do e <- genExpr (size - 1) ct ctx t
     el <- Control.Monad.mapM (genExpr (size `div` 2) ct ctx) pt
     return (MethodInvk e m el) 

-- Function: genCast
-- Objective: Generate an expression using Cast constructor.
-- Params: Size, Context, Class table, Class name, Class name.
-- Returns: A Cast expression.
--------------------------------------------------------------
genCast :: Int -> Env -> CT -> String -> String -> Gen Expr
genCast size ctx ct t tc = do e <- genExpr (size - 1) ct ctx tc
                              return (Cast t e)

-- Function: cfields
-- Objective: Generate a list of candidates for FieldAccess constructor.
-- Params: Size, Context, Class table, Class name.
-- Returns: A list containing FieldAccess expressions of a given type.
----------------------------------------------------------------------
cfields :: Int -> Env -> CT -> String -> [Gen Expr]
cfields size ctx ct t =
  Data.List.concatMap 
    (\k -> case (fields k ct) of
             Just lst -> Data.List.concatMap (\((TypeClass t'),f) -> 
                           if (t == t') then
                             [genFieldAccess size ctx ct k f]
                           else
                             []        
                         ) lst
             _ -> []
    ) (keys ct) 

-- Function: cmethods
-- Objective: Generate a list of candidates for MethodInvk constructor.
-- Params: Size, Context, Class table, Class name.
-- Returns: A list containing MethodInvk expressions of a given type.
-----------------------------------------------------------------------
cmethods :: Int -> Env -> CT -> String -> [Gen Expr]
cmethods size ctx ct t = 
  Data.List.concatMap
    (\k -> case (methods k ct) of
             Just lst -> 
               Data.List.concatMap (\(Method (TypeClass t') m pt _) ->
                 if (t == t') then
                   let pt' = Data.List.map (\(TypeClass t'',_) -> t'') pt
                     in [genMethodInvk size ctx ct k m pt']
                 else
                   []
               ) lst
             _ -> []
    ) (keys ct)


-- Function: cucast
-- Objective: Generate a list of candidates for Cast constructor.
-- Params: Size, Context, Class table, Class name.
-- Returns: A list containing Upcast candidates of a given type.
-----------------------------------------------------------------
cucast :: Int -> Env -> CT -> String -> [Gen Expr]
cucast size ctx ct t = 
  Data.List.concatMap 
    (\k -> if (subtyping k t ct) then
             [genCast size ctx ct t k]
           else 
             []  
    ) (keys ct)

-- Function: cdcast
-- Objective: Generate a list of candidates for Cast constructor.
-- Params: Size, Context, Class table, Class name.
-- Returns: A list containing Downcast candidates of a given type.
------------------------------------------------------------------
cdcast :: Int -> Env -> CT -> String -> [Gen Expr]
cdcast size ctx ct t = 
  Data.List.concatMap
    (\k -> if (k /= t && subtyping t k ct) then
             [genCast size ctx ct t k]
           else
             []
    ) (keys ct)

-- Function: cscast
-- Objective: Generate a list of candidates for Cast constructor.
-- Params: Size, Context, Class table, Class name.
-- Returns: A list containing Stupid cast candidates of a given type.
---------------------------------------------------------------------
cscast :: Int -> Env -> CT -> String -> [Gen Expr]
cscast size ctx ct t = 
  Data.List.concatMap
    (\k -> if (not (subtyping t k ct) && not (subtyping k t ct)) then
             [genCast size ctx ct t k]
           else
             []
    ) (keys ct)

-- Function: genExpr
-- Objective: Generate an expression of a given type.
-- Params: Size, Class table, Context, Class name.
-- Returns: An expression of a given type.
-----------------------------------------------------
genExpr :: Int -> CT -> Env -> String -> Gen Expr
genExpr size ct ctx t 
  | size > 0 = 
      oneof ([do el <- Control.Monad.mapM (genExpr (size `div` 2) ct ctx) ft
                 return (CreateObject t el)] ++ 
              maybeToList (pickVar ctx (TypeClass t)) ++ 
              fc ++ mc ++ ucc ++ dcc ++ scc)
  | otherwise = do el <- Control.Monad.mapM (genExpr (size `div` 2) ct ctx) ft
                   oneof ([return (CreateObject t el)] ++ 
                       maybeToList (pickVar ctx (TypeClass t)))
    where ft = case (fields t classtable) of
                 Just lst -> Data.List.map (\((TypeClass t'),n') -> t') lst
                 _ -> [] 
          fc = if (Data.List.length (cfields size ctx ct t) > 0) then
                 [oneof (cfields (size - 1) ctx ct t)]
               else []
          mc = if (Data.List.length (cmethods size ctx ct t) > 0) then
                 [oneof (cmethods (size - 1) ctx ct t)]
               else 
                 []
          ucc = if (Data.List.length (cucast size ctx ct t) > 0) then
                  [oneof (cucast (size - 1) ctx ct t)]
                else 
                  []
          dcc = if (Data.List.length (cdcast size ctx ct t) > 0) then
                  [oneof (cdcast (size - 1) ctx ct t)]
                else 
                  []
          scc = if (Data.List.length (cscast size ctx ct t) > 0) then
                  [oneof (cscast (size - 1) ctx ct t)]
                else
                  []

-- Function: genType
-- Objective: Generate a type from the class table.
-- Params: None.
-- Returns: A string containing a class name.
---------------------------------------------------
genType :: Gen String
genType = elements (keys classtable)

-- Function: genExpression
-- Objective: Generate an expression of a given type.
-- Params: Class name.
-- Returns: An expression of a given type.
-----------------------------------------------------
genExpression :: String -> Gen Expr
genExpression t = sized (\n -> genExpr n classtable Data.Map.empty t)

-- Type-class: Arbitrary Expr
-- Objective: Generate random well-typed expressions.
-----------------------------------------------------
instance Arbitrary Expr where
  arbitrary = do t <- elements (keys classtable)
                 sized (\n -> genExpr n classtable Data.Map.empty t)

-- Function: prop_genwelltyped
-- Objective: Test if a generated expression is well-typed.
-- Params: None.
-- Returns: True if the expression is well-typed, False otherwise.
------------------------------------------------------------------
prop_genwelltyped = forAll genType $ 
                      \t -> forAll (genExpression t) $ 
                      \e -> either (const False)
                                   (\(TypeClass t') -> t == t')
                                   (typeof Data.Map.empty classtable e)

