-- Arbitrary class table for testing Featherweight Java interpreter.
-- Author: Samuel da Silva Feitosa
-- Date: 03/2018
--------------------------------------------------------------------
module CT where
import FJParser
import Data.Map

{-
class A extends Object {
  Object a;
  A(Object a) {
    super();
    this.a = a;
  }
  Object getA() {
    return this.a;
  }
}
-}

classA = Class "A" "Object" -- Base and derived class
           [(TypeClass "Object", "a")] -- Attributes
           (Constr "A" [(TypeClass "Object", "a")] [] [("a", "a")]) -- Constructor
           [(Method (TypeClass "Object") "getA" [] (FieldAccess (Var "this") "a"))] -- Methods

{-
class B extends Object {
  Object b;
  B(Object b) {
    super();
    this.b = b;
  }
  Object getB() {
    return this.b;
  }
  B setB(Object b) {
    return new B(b);
  }   
}
-}

classB = Class "B" "Object" -- Base and derived class
           [(TypeClass "Object", "b")] -- Attributes
           (Constr "B" [(TypeClass "Object", "b")] [] [("b", "b")]) -- Constructor
           [(Method (TypeClass "Object") "getB" [] (FieldAccess (Var "this") "b")),
            (Method (TypeClass "B") "setB" [(TypeClass "Object", "b")] (CreateObject "B" [(Var "b")]))] -- Methods

{-
class C extends Object {
  A a;
  B b;
  C(A a, B b) {
    super();
    this.a = a;
    this.b = b;
  }
  A getA() {
    return this.a;
  }
  Object getAData() {
    return this.a.getA();
  }
  Object getBData() {
    return this.b.b;
  }
}
-}

classC = Class "C" "Object" -- Base and derived class
           [(TypeClass "A", "a"), -- Attributes
            (TypeClass "B", "b")] 
           (Constr "C" [(TypeClass "A", "a"),(TypeClass "B", "b")] [] [("a","a"), ("b","b")]) -- Constructor
           [(Method (TypeClass "A") "getA" [] (FieldAccess (Var "this") "a")), -- Methods
            (Method (TypeClass "Object") "getAData" [] (MethodInvk (FieldAccess (Var "this") "a") "getA" [])),
            (Method (TypeClass "Object") "getBData" [] (FieldAccess (FieldAccess (Var "this") "b") "b"))] 

{-
class D extends Object {
  A a;
  B b;
  D(A a, B b) {
    super();
    this.a = a;
    this.b = b;
  }
  D setAB(Object a, Object b) {
    return new D(new A(a), this.b.setB(b));
  }
}
-}

classD = Class "D" "Object" -- Base and derived class
           [(TypeClass "A", "a"), -- Attributes
            (TypeClass "B", "b")] 
           (Constr "D" [(TypeClass "A", "a"),(TypeClass "B", "b")] [] [("a","a"), ("b","b")]) -- Constructor
           [(Method (TypeClass "D") "setAB" [(TypeClass "Object", "a"),(TypeClass "Object", "b")] (CreateObject "D" [(CreateObject "A" [(Var "a")]),(MethodInvk (FieldAccess (Var "this") "b") "setB" [(Var "b")])]))] 

{-
class E extends Object {
  Object a;
  Object b;
  E(Object a, Object b) {
    super();
    this.a = a;
    this.b = b;
  }
  Object getA() {
    return this.a;
  }
  Object getB() {
    return this.b;
  }
}
-}

classE = Class "E" "Object" -- Base and derived class
           [(TypeClass "Object", "a"),(TypeClass "Object", "b")] -- Attributes
           (Constr "A" [(TypeClass "Object", "a"),(TypeClass "Object", "b")] [] [("a", "a"),("b", "b")]) -- Constructor
           [(Method (TypeClass "Object") "getA" [] (FieldAccess (Var "this") "a")), -- Methods
           (Method (TypeClass "Object") "getB" [] (FieldAccess (Var "this") "b"))] 


classtable = Data.Map.fromList [("A", classA), ("B", classB), ("C", classC), ("D", classD), ("E", classE)]
