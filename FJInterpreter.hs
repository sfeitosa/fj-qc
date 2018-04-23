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
            maybe (Nothing)
                  (\idx -> Just (p !! idx))
                  (Data.List.findIndex (\(tp,nm) -> f == nm) flds)
          _ -> Nothing
  else -- RC-Field
    maybe (Nothing)
          (\e' -> Just (FieldAccess e' f))
          (eval' ct e) 
eval' ct (MethodInvk e m p) =
  if (isValue ct e) then 
    -- R-Invk-Arg
    let p' = Data.List.map (\x -> case (eval' ct x) of Just x' -> x') p in 
      case e of -- R-Invk
        (CreateObject c cp) -> 
          maybe (Nothing)
                (\(fpn, e') -> subst (fpn ++ ["this"])  (p' ++ [e]) e')
                (mbody m c ct)
        _ -> Nothing      
  else -- R-Invk-Recv
    maybe (Nothing)
          (\e' -> Just (MethodInvk e' m p))
          (eval' ct e)
eval' ct (Cast c e) = 
  if (isValue ct e) then -- R-Cast
    case e of 
      (CreateObject c' p) -> if (subtyping c' c ct) then
                               Just (CreateObject c' p)
                             else
                               Nothing
      _ -> Nothing
  else -- RC-Cast
    maybe (Nothing)
          (\e' -> Just (Cast c e'))
          (eval' ct e)

-- Function: subst
-- Objective: Replace actual parameters in method body expression. 
-- Params: List of formal parameters names, List of actual parameters,
-- Method body expression.
-- Returns: A new changed expression.
-------------------------------------------
subst :: [String] -> [Expr] -> Expr -> Maybe Expr
subst p v (Var x) = maybe (Nothing)
                          (\idx -> Just (v !! idx))
                          (Data.List.elemIndex x p)
subst p v (FieldAccess e f) = maybe (Nothing) 
                                    (\e' -> Just (FieldAccess e' f)) 
                                    (subst p v e)
subst p v (MethodInvk e n ap) = 
  let ap' = Data.List.map (\x -> case (subst p v x) of Just x' -> x') ap in
    maybe (Nothing)
          (\e' -> Just (MethodInvk e' n ap'))
          (subst p v e)
subst p v (CreateObject c ap) = 
  let ap' = Data.List.map (\x -> case (subst p v x) of Just x' -> x') ap in
    Just (CreateObject c ap')
subst p v (Cast c e) = maybe (Nothing)
                             (\e' -> Just (Cast c e'))
                             (subst p v e)


