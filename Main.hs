-- TODO: printing

{-# LANGUAGE ParallelListComp, PatternGuards #-}
module Main where

import System.Environment (getArgs)

-- Choose only one...
import Parser
import Text.Parsec

import Printer

import Debug.Trace

--substitution storing list of term pairs
type Substitution = [(Term, Term)]

--termUnify:: Term -> Term -> Substitution
--termUnify (Atom a1) (Atom a2) = if a1 == a2 then [] else Nothing
--termUnify (Variable v1) (Variable v2) = if v1 == v2 then [] else Nothing
--termUnify (String s1) (String s2) = if s1 == s2 then [] else Nothing
--termUnify (Variable x) term2 = if x 'elem' term2 then [(Variable x, term2)] else Nothing
--termUnifty term1 (Variable x) = if x 'elem' term1 then [(Variable x, term1)] else Nothing

main :: IO ()
main = go . concat =<< mapM readFile =<< getArgs

go :: String -> IO ()
go s = putStrLn "Your implementation starts here!"
