module L2.Syntax where

import qualified Data.Map as M

type Program
  = M.Map ClassId ClassDef
data ClassDef
  = ClassDef
  { parentClass :: ClassId
  , defFields   :: M.Map FieldId Type
  , defMethods  :: M.Map MethId Method
  }
data Method
  = Method
  { retType   :: Type
  , methName  :: MethId
  , paramType :: Type
  , methBody  :: Expr
  }
data Type
  = Class ClassId
  | Bool
data Expr
  = If Expr Expr Expr
  | FieldAccess Expr FieldId
  | FieldAssign Expr FieldId Expr
  | MethCall Expr MethId Expr
  | New ClassId
  | X
  | This
  | TrueE  -- prevent name clash
  | FalseE -- prevent name clash
  | NullE

type ClassId = String
type FieldId = String
type MethId = String
