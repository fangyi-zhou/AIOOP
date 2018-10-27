module L1.Lookup where

import qualified Data.Map as M

import L1.Syntax

field :: Program -> ClassId -> FieldId -> Type
field p c f
  = fst (p M.! c) M.! f

fields :: Program -> ClassId -> [FieldId]
fields p c
  = M.keys $ fst $ p M.! c

method :: Program -> ClassId -> MethId -> Method
method p c m
  = snd (p M.! c) M.! m
