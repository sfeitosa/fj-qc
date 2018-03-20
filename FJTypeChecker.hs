-- Haskell type checker for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 01/2018
----------------------------------------------
module FJTypeChecker where
import FJParser
import FJUtils
import Data.Map
import Data.List

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

-- Function: typeof
-- Objective: Check the type of a given expression.
-- Params: Type context, Class table, Expression.
-- Returns: The type of a given term or a type error.
-----------------------------------------------------
typeof :: Env -> CT -> Expr -> Either TypeError Type
typeof ctx ct (Var v) = -- T-Var
  case (Data.Map.lookup v ctx) of 
    Just t -> return t
    _ -> throwError (VariableNotFound v) 
typeof ctx ct (FieldAccess e f) = -- T-Field
  case (typeof ctx ct e) of
    Right (TypeClass c) ->
      case (fields c ct) of 
        Just flds ->
          case (Data.List.find (\(tp,nm) -> f == nm) flds) of
            Just (tp,nm) -> return tp
            _ -> throwError (FieldNotFound f)
        _ -> throwError (ClassNotFound c)
    e -> e -- Error: Expression type not found
typeof ctx ct (MethodInvk e m p) = -- T-Invk
  case (typeof ctx ct e) of
    Right (TypeClass c) -> 
      case (mtype m c ct) of 
        Just (pt, rt) -> 
          let tmp = Data.List.zipWith (\e t -> (e, t)) p pt in
            if (Data.List.all (\(e, TypeClass t') -> 
                  case (typeof ctx ct e) of
                    Right (TypeClass tp') -> subtyping tp' t' ct
                    _ -> False
               ) tmp) then
              return rt
            else
              throwError (ParamsTypeMismatch tmp) 
        _ -> throwError (MethodNotFound m c)
    e -> e -- Error: Expression type not found
typeof ctx ct (CreateObject c p) = -- T-New
  case (fields c ct) of 
    Just flds -> 
      let tmp = Data.List.zipWith (\e (tp,_) -> (e,tp)) p flds in
        if (Data.List.all (\(e,TypeClass t) -> 
             case (typeof ctx ct e) of 
               Right (TypeClass tp') -> subtyping tp' t ct
               _ -> False
           ) tmp) then
          return (TypeClass c)
        else 
          throwError (ParamsTypeMismatch tmp)
    _ -> throwError (ClassNotFound c)
typeof ctx ct (Cast c e) = 
  case (typeof ctx ct e) of 
    Right (TypeClass tp)  
      | (subtyping tp c ct) -> return (TypeClass c) -- T-UCast
      | (subtyping c tp ct) && (c /= tp) -> return (TypeClass c) -- T-DCast
      | (not (subtyping c tp ct)) && (not (subtyping tp c ct)) -> 
          return (TypeClass c) -- T-SCast
      | otherwise -> throwError (WrongCast c e)
    e -> e -- Error: Expression type not found

-- Function: methodTyping
-- Objective: Check if a method is well formed.
-- Params: Method, Class, Type context, Class table.
-- Returns: True for a well formed method, False otherwise.
-----------------------------------------------------------
methodTyping :: Env -> CT -> Class -> Method -> Bool
methodTyping ctx ct (Class c b _ _ _) (Method (TypeClass r) m p e) = 
  let p' = Data.List.map (\(t,n) -> (n,t)) p 
      ctx' = Data.Map.union (Data.Map.fromList p') ctx
      ctx'' = Data.Map.insert "this" (TypeClass c) ctx' in
    case (typeof ctx'' ct e) of 
      Right (TypeClass tp) -> 
        if (subtyping tp r ct) then
          case (mtype m b ct) of 
            Just (pm, rm) -> 
              (pm == (Data.List.map (\(t,n) -> t) p)) && (rm == (TypeClass r))
            _ -> True
        else 
          False
      _ -> False

-- Function: classTyping
-- Objective: Check if a class is well formed.
-- Params: Class, Type context, Class table.
-- Returns: True for a well formed class, False otherwise.
----------------------------------------------------------
classTyping :: Class -> Env -> CT -> Bool
classTyping cl@(Class c b attrs (Constr cn pc s ths) meths) ctx ct = 
  case (fields b ct) of 
    Just flds -> 
      if (pc == (flds ++ attrs)) then
        if (Data.List.all (\(n',n'') -> n' == n'') ths) then
          let p' = Data.List.map (\(tp, nm) -> nm) pc 
              p'' = s ++ (Data.List.map (\(n',n'') -> n') ths) 
            in (p' == p'') && (Data.List.all (methodTyping ctx ct cl) meths)
        else 
          False
      else 
         False
    _ -> False

