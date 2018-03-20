-- QuickCheck property-based testing for Featherweight Java
-- Author: Samuel da Silva Feitosa
-- Date: 03/2018
-----------------------------------------------------------
module Tests where
import FJParser
import FJUtils
import FJInterpreter
import FJTypeChecker
import Test.QuickCheck
import Data.Map
import Data.List
import Data.Maybe
import Control.Monad

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

-- Function: genClassName
-- Objective: Generate a random class name.
-- Params: None.
-- Returns: A string containing a valid class name.
--------------------------------------------------
genClassName :: Gen String
genClassName = do n <- elements ['A' .. 'Z']
                  return [n]

-- Function: genType
-- Objective: Generate a type from the class table.
-- Params: None.
-- Returns: A string containing a class name.
---------------------------------------------------
genType :: CT -> Gen String
genType ct = elements ("Object" : keys ct)

-- Function: genVar
-- Objective: Generate a random variable name.
-- Params: None
-- Returns: A string containing a valid variable name.
-----------------------------------------------------
genVar :: Gen String
genVar = do n <- elements ['a' .. 'z']
            return [n]

-- Function: genAttrs
-- Objective: Generate a list of type/attributes for a given class.
-- Params: Class table, class name, base class.
-- Returns: A list of type/attributes.
-------------------------------------------------------------------
genAttrs :: CT -> String -> String -> Gen [(String,String)]
genAttrs ct c b = 
  do n <- choose (0,5)
     tl <- vectorOf n (genType ct)
     vl <- (vectorOf n (genVar))
     tl' <- Control.Monad.mapM (\n -> if (subtyping n c ct) then 
                                        return "Object"
                                      else 
                                        return n
                                ) tl
     case (fields b ct) of 
       Just bflds -> return (zip tl' 
                       ((nub vl) Data.List.\\ (snd (unzip bflds))))
       _ -> return (zip tl' (nub vl))

-- Function: genMethod
-- Objective: Generate a random method for a given class.
-- Params: Class table, class name, base class.
-- Returns: A generated method.
---------------------------------------------------------
genMethod :: CT -> String -> String -> Gen Method
genMethod ct c b = 
  do n <- choose (0,3) -- Number of params
     tl <- vectorOf n (genType ct) -- Type of params
     vl <- vectorOf n (genVar) -- Name of params
     m <- genVar -- Method name
     r <- genType ct -- Return type 
     e <- genExpression ct -- Body expression
            (fromList (("this", TypeClass c) :
              (zip (nub vl) (Data.List.map (\t -> TypeClass t) tl)))
            ) r
     return (Method 
               (TypeClass r) 
               ('m':m)  
               (zip (Data.List.map (\t -> TypeClass t) tl) (nub vl))
               e
            )

-- Function: genMethods
-- Objective: Generate a list of random methods.
-- Params: Class table, class name, base class.
-- Returns: A list of methods.
------------------------------------------------
genMethods :: CT -> String -> String -> Gen [Method]
genMethods ct c b = 
 do n <- choose (0,3) -- Number of methods
    ml <- vectorOf n (genMethod ct c b) 
    return (deleteFirstsBy (\(Method _ n' _ _) (Method _ n'' _ _) -> n' == n'') 
             (nubBy (\(Method _ n' _ _) (Method _ n'' _ _) -> n' == n'') ml)
              bml)
  where bml = case (methods b ct) of 
                Just meths -> meths
                _ -> []

-- Function: formatConstr
-- Objective: Format a class constructor.
-- Params: Class table, class name, base class, class attributes.
-- Returns: A constructor AST for the given class. 
-----------------------------------------------------------------
formatConstr :: CT -> String -> String -> [(Type,String)] -> Constr
formatConstr ct c b attrs = 
  case (fields b ct) of 
    Just flds -> Constr c -- Class name
                        (flds ++ attrs) -- Constr parameters
                        (snd (unzip flds)) -- Super parameters
                        (zip (snd (unzip attrs)) (snd (unzip attrs))) -- this
    _ -> Constr c attrs [] (zip (snd (unzip attrs)) (snd (unzip attrs)))

-- Function: genClass
-- Objective: Generate a random class with attributes, constructor and methods.
-- Params: Class table, class name, base class.
-- Returns: A generated class.
-------------------------------------------------------------------------------
genClass :: CT -> String -> String -> Gen Class
genClass ct c b = 
  do attrs <- genAttrs ct c b
     meths <- genMethods ct c b
     return (Class c b 
              (Data.List.map (\(k,p) -> (TypeClass k,p)) attrs) -- Attributes
              (formatConstr ct c b -- Constructor
                (Data.List.map (\(k,p) -> (TypeClass k,p)) attrs)) -- Params
              meths) -- Methods

-- Function: addClass
-- Objective: Generate a class with the given name and adds to the class table.
-- Params: Class table, class name.
-- Returns: A class table containing the generated class.
-------------------------------------------------------------------------------
addClass :: CT -> String -> Gen CT
addClass ct nm = do cb <- genType ct
                    cl <- genClass ct nm cb
                    return (Data.Map.insert nm cl ct)

-- Function: genClassTable
-- Objective: Generate a random class table.
-- Params: None.
-- Returns: A random class table.
--------------------------------------------
genClassTable :: Gen CT
genClassTable = do n <- choose (1,10)
                   ns <- vectorOf n (genClassName)
                   ct <- Control.Monad.foldM (addClass) Data.Map.empty (nub ns)
                   return ct
                   

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
    ) ("Object" : keys ct) 

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
    ) ("Object" : keys ct)


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
    ) ("Object" : keys ct)

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
              fc ++ mc ++ ucc)
  | otherwise = do el <- Control.Monad.mapM (genExpr (size `div` 2) ct ctx) ft
                   oneof ([return (CreateObject t el)] ++ 
                       maybeToList (pickVar ctx (TypeClass t)))
    where ft = case (fields t ct) of
                 Just lst -> Data.List.map (\((TypeClass t'),n') -> t') lst
                 _ -> []
          -- FieldAccess candidates 
          fc = if (Data.List.length (cfields size ctx ct t) > 0) then
                 [oneof (cfields (size - 1) ctx ct t)]
               else []
          -- MethodInvk candidates
          mc = if (Data.List.length (cmethods size ctx ct t) > 0) then
                 [oneof (cmethods (size - 1) ctx ct t)]
               else 
                 []
          -- Upcast candidates
          ucc = if (Data.List.length (cucast size ctx ct t) > 0) then
                  [oneof (cucast (size - 1) ctx ct t)]
                else 
                  []
          -- Downcast candidates
          dcc = if (Data.List.length (cdcast size ctx ct t) > 0) then
                  [oneof (cdcast (size - 1) ctx ct t)]
                else 
                  []
          -- Stupid cast candidates (not used for testing properties)
          scc = if (Data.List.length (cscast size ctx ct t) > 0) then
                  [oneof (cscast (size - 1) ctx ct t)]
                else
                  []

-- Function: genExpression
-- Objective: Generate an expression of a given type.
-- Params: Class name.
-- Returns: An expression of a given type.
-----------------------------------------------------
genExpression :: CT -> Env -> String -> Gen Expr
genExpression ct ctx t = sized (\n -> genExpr n ct ctx t)

-- Type-class: Arbitrary Expr
-- Objective: Generate random well-typed expressions.
-----------------------------------------------------
instance Arbitrary Expr where
  arbitrary = do ct <- genClassTable
                 t <- elements ("Object" : keys ct)
                 sized (\n -> genExpr n ct Data.Map.empty t)

-- Function: prop_genwelltypedct
-- Objective: Test if the generated class table is well-formed.
-- Params: None.
-- Returns: True if the class tables are well-formed, False otherwise.
----------------------------------------------------------------------
prop_genwellformedct =
  forAll (genClassTable) $
    \ct -> Data.List.all 
             (\(c,cl) -> classTyping cl Data.Map.empty ct) (Data.Map.toList ct)

-- Function: prop_gencastsafeexpr
-- Objective: Test if the generated expressions are cast-safe.
-- Params: None.
-- Returns: True if the expression is cast-safe, False otherwise.
-----------------------------------------------------------------
prop_gencastsafeexpr = 
  forAll (genClassTable) $ 
    \ct -> forAll (genType ct) $
    \t -> forAll (genExpression ct Data.Map.empty t) $
    \e -> case e of 
            (Cast c e) -> case (typeof Data.Map.empty ct e) of
                            Right (TypeClass t') -> subtyping t' c ct
                            _ -> False
            _ -> True

-- Function: prop_genwelltypedexpr
-- Objective: Test if the generated expressions are well-typed.
-- Params: None.
-- Returns: True if the expressions are well-typed, False otherwise.
--------------------------------------------------------------------
prop_genwelltypedexpr =
  forAll (genClassTable) $
    \ct -> forAll (genType ct) $ 
    \t -> forAll (genExpression ct Data.Map.empty t) $ 
    \e -> either (const False)
                 (\(TypeClass t') -> t == t')
                 (typeof Data.Map.empty ct e)

-- Function: prop_progress
-- Objective: Tests for the property of progress.
-- Params: None.
-- Returns: True if the property held for the test casts, False otherwise.
-------------------------------------------------------------------------- 
prop_progress =
  forAll (genClassTable) $
    \ct -> forAll (genType ct) $
    \t -> forAll (genExpression ct Data.Map.empty t) $
    \e -> isValue ct e || maybe (False) (const True) (eval' ct e)

-- Function: prop_preservation
-- Objective: Tests for the property of preservation.
-- Params: None.
-- Returns: True if the property held for the test cases, False otherwise.
--------------------------------------------------------------------------
prop_preservation =
  forAll (genClassTable) $
    \ct -> forAll (genType ct) $ 
    \t -> forAll (genExpression ct Data.Map.empty t) $
    \e -> either (const False)
                 (\(TypeClass t') -> subtyping t' t ct)
                 (case (eval' ct e) of 
                    Just e' -> typeof Data.Map.empty ct e'
                    _ -> throwError (UnknownError e)
                 )

