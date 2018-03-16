-- Haskell interpreter for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 01/2018
---------------------------------------------
module FJInterpreter where
import FJParser
import FJUtils
import Data.Maybe
import Data.List

-- Function: eval'
-- Objective: Evaluate an expression.
-- Params: Class table, Expression.
-- Returns: An expression after processing one reduction step.
--------------------------------------------------------------
eval' :: CT -> Expr -> Maybe Expr
eval' ct (CreateObject c p) = -- RC-New-Arg
  let p' = Data.List.map (\x -> case (eval' ct x) of Just x' -> x') p
    in Just (CreateObject c p')
eval' ct (FieldAccess e f) = 
  if (isValue ct e) then -- R-Field
    case e of 
      (CreateObject c p) ->
        case (fields c ct) of
          Just flds -> 
            case (Data.List.findIndex (\(tp,nm) -> f == nm) flds) of
              Just idx -> Just (p !! idx)
  else -- RC-Field
    case (eval' ct e) of 
      Just e' -> Just (FieldAccess e' f) 
      _ -> Nothing
eval' ct (MethodInvk e m p) =
  if (isValue ct e) then 
    -- R-Invk-Arg
    let p' = Data.List.map (\x -> case (eval' ct x) of Just x' -> x') p in 
      case e of -- R-Invk
        (CreateObject c cp) -> 
          case (mbody m c ct) of
            Just (fpn, e') -> subst (fpn ++ ["this"])  (p' ++ [e]) e'
        _ -> Nothing      
  else -- R-Invk-Recv
    case (eval' ct e) of 
      Just e' -> Just (MethodInvk e' m p)
      _ -> Nothing
eval' ct (Cast c e) = 
  if (isValue ct e) then -- R-Cast
    case e of 
      (CreateObject c' p) -> if (subtyping c' c ct) then
                               Just (CreateObject c' p)
                             else
                               Nothing
  else -- RC-Cast
    case (eval' ct e) of
      Just e' -> Just (Cast c e')
      _ -> Nothing  
eval' _ _ = Nothing


-- Function: subst
-- Objective: Replace actual parameters in method body expression. 
-- Params: List of formal parameters names, List of actual parameters,
-- Method body expression.
-- Returns: A new changed expression.
-------------------------------------------
subst :: [String] -> [Expr] -> Expr -> Maybe Expr
subst p v (Var x) = case (Data.List.elemIndex x p) of 
                      Just idx -> Just (v !! idx)
subst p v (FieldAccess e f) = case (subst p v e) of
                                Just e' -> Just (FieldAccess e' f)
subst p v (MethodInvk e n ap) = 
  let ap' = Data.List.map (\x -> case (subst p v x) of Just x' -> x') ap in
    case (subst p v e) of 
      Just e' -> Just (MethodInvk e' n ap')
subst p v (CreateObject c ap) = 
  let ap' = Data.List.map (\x -> case (subst p v x) of Just x' -> x') ap in
    Just (CreateObject c ap')
subst p v (Cast c e) = case (subst p v e) of 
                         Just e' -> Just (Cast c e')

