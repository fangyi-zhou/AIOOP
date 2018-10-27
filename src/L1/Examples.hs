module L1.Examples where

import qualified Data.Map as M

import L1.Syntax
import L1.Runtime


book :: ClassDef
book
  = (fields, methods)
  where
    fields = M.fromList [("good", Bool), ("owner", Class "Person")]
    methods = M.fromList [("readBy", Method Bool "readBy" (Class "Person") body)]
    body = FieldAssign This "good" TrueE

person :: ClassDef
person
  = (fields, methods)
  where
    fields = M.fromList [("like", Class "Book")]
    methods = M.fromList [("meet", Method (Class "Book") "meet" (Class "Person") body)]
    body = FieldAssign This "like" (FieldAccess X "like")

phi0 :: StackFrame
phi0 = (3, Address 4)

chi0 :: Heap
chi0
  = M.fromList [
    (3, ("Book", M.fromList [("good", FalseV), ("owner", Address 5)]))
  , (4, ("Person", M.fromList [("like", Address 3)]))
  , (5, ("Person", M.fromList [("like", NullV)]))
  ]
