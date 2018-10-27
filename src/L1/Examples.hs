module L1.Examples where

import qualified Data.Map as M

import L1.Syntax


book :: ClassDef
book
  = (fields, methods)
  where
    fields = M.fromList [("good", Bool), ("owner", Class "Person")]
    methods = M.fromList [("readBy", Method Bool "readBy" (Class "Person") body)]
    body = FieldAssign This "good" True_

person :: ClassDef
person
  = (fields, methods)
  where
    fields = M.fromList [("like", Class "Book")]
    methods = M.fromList [("meet", Method (Class "Book") "meet" (Class "Person") body)]
    body = FieldAssign This "like" (FieldAccess X "like")
