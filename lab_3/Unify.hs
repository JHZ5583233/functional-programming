module Unify (mgu, applyUnifier) where

mgu :: FuncApplication -> FuncApplication -> Maybe Unifier

applyUnifier :: Unifier -> FuncApplication -> FuncApplication