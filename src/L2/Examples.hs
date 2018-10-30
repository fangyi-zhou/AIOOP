module L2.Examples where

import qualified Data.Map   as M

import           L2.Runtime
import           L2.Syntax

student :: ClassDef
student = ClassDef "Object" M.empty methods
  where
    methods = M.fromList [("eat", Method Bool "eat" (Class "Food") (FieldAssign X "tasty" TrueE))]

food :: ClassDef
food = ClassDef "Object" fields methods
  where
    fields = M.fromList [("fat", Bool)]
    methods = M.fromList [tasty, mix]
    tasty = ("tasty", Method Bool "tasty" Bool FalseE)
    mix = ("mix", Method (Class "Food") "mix" (Class "Food") X)
