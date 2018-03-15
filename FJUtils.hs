-- Utilitary functions for Featherweight Java interpreter
-- Author: Samuel da Silva Feitosa
-- Date: 01/2018
---------------------------------------------------------
module FJUtils where
import FJParser
import Data.Map
import Data.List

-- Function: subtyping
-- Objective: Check classes for subtyping.
-- Params: Class A, Class B, Class table
-- Returns: Returns if class A is subtype of class B.
-----------------------------------------------------
subtyping :: String -> String -> CT -> Bool
subtyping _ "Object" _ = True
subtyping "Object" _ _ = False
subtyping c c' ct 
    | c == c' = True
    | otherwise = case (Data.Map.lookup c ct) of
                    Just (Class _ c'' _ _ _) -> 
                      if c' == c'' then
                        True
                      else
                        subtyping c'' c' ct
                    _ -> False

-- Function: fields
-- Objective: Search for a class on class table and returns its fields.
-- Params: Class name, Class table.
-- Returns: A monad Maybe containing the field list or Nothing.
-----------------------------------------------------------------------
fields :: String -> CT -> Maybe [(Type,String)]
fields "Object" _ = Just []
fields c ct = case (Data.Map.lookup c ct) of 
                Just (Class _ c'' attrs _ _) ->
                  case (fields c'' ct) of 
                    Just base -> Just (base ++ attrs)
                    _ -> Nothing
                _ -> Nothing

-- Function: methods
-- Objective: Search for a class on class table and returns its methods.
-- Params: Class name, Class table.
-- Returns: A monad Maybe containing the methd list of Nothing.
------------------------------------------------------------------------
methods :: String -> CT -> Maybe [Method]
methods "Object" _ = Just []
methods c ct = case (Data.Map.lookup c ct) of 
                 Just (Class _ c'' _ _ meths) ->
                   case (methods c'' ct) of
                     Just bmeths -> Just (meths ++ bmeths)
                     _ -> Nothing
                 _ -> Nothing


-- Function: mtype
-- Objective: Search for a class on class table, then looks up for a method 
-- and returns its type.
-- Params: Method name, Class name, Class table.
-- Returns: A monad Maybe containing the method type.
---------------------------------------------------------------------------
mtype :: String -> String -> CT -> Maybe ([Type], Type)
mtype _ "Object" _ = Nothing
mtype m c ct = 
  case (Data.Map.lookup c ct) of
    Just (Class _ c' _ _ meths) ->
      case (Data.List.find (\(Method _ n _ _) -> m == n) meths) of
        Just (Method r _ p _) -> Just (Data.List.map (\(tp, nm) -> tp) p, r)
        _ -> mtype m c' ct
    _ -> Nothing

-- Function: mbody
-- Objective: Search for a class on class table, then looks up for a method
-- and returns its body.
-- Params: Method name, Class name, Class table.
-- Returns: A monad Maybe containing the method body or Nothing.
---------------------------------------------------------------------------
mbody :: String -> String -> CT -> Maybe ([String], Expr)
mbody _ "Object" _ = Nothing
mbody m c ct = 
  case (Data.Map.lookup c ct) of
    Just (Class _ c' _ _ meths) -> 
      case (Data.List.find (\(Method _ n _ _) -> m == n) meths) of
        Just (Method _ _ p e) -> Just (Data.List.map (\(tp, nm) -> nm) p, e)
        _ -> mbody m c' ct
    _ -> Nothing

-- Function: isValue 
-- Objective: Check if an expression represents a value.
-- Params: Expression, Class table.
-- Returns: Boolean indicating if an expression is a value.
-----------------------------------------------------------
isValue :: CT -> Expr -> Bool
isValue _ (CreateObject c []) = True
isValue ct (CreateObject c p) = Data.List.all (isValue ct) p
isValue _ _ = False

