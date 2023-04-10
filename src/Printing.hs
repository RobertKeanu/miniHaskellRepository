
module Printing (showExp) where

import Exp
import Data.List (intercalate)

showVar :: Var -> String
showVar x = show x

showExp :: ComplexExp -> String
showExp (CX v) = showVar v 
showExp (Nat x) =show x
showExp (CLam v cexp) = "\\" ++ (showVar v) ++ "->" ++ (showExp cexp)
showExp (CApp c1 c2) = "(" ++ showExp c1 ++ " " ++ showExp c2 ++ ")"
showExp (Let v c1 c2) = "(let " ++ showVar v ++ " := " ++ showExp c1 ++ " in " ++ showExp c2  ++ ")"
showExp (LetRec v c1 c2) = " (letrec " ++ showVar v
showExp (List l) = "[" ++ intercalate ", " (map showExp l) ++ "]"