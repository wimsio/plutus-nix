{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellDemo(
        makeHello, 
        makeAddFunction, 
        makeVolFunction,
        makeGradeFunc,
        mkTriple) where

import Language.Haskell.TH 

makeHello :: String -> Q [Dec]
makeHello name = [d| greet = putStrLn ("Hello " ++ name)|]

makeAddFunction :: Int -> Int -> Q [Dec]
makeAddFunction a b = [d|add = a + b|]

makeVolFunction :: Q [Dec]
makeVolFunction  = [d|
  volFunction :: Int -> Int -> Int -> Int
  volFunction x b z = x * b * z
  |]

makeGradeFunc :: Q [Dec]
makeGradeFunc = [d|
  gradeFunc :: Int -> String
  gradeFunc n
    | n < 25 = "Grade is F"
    | n < 45 = "Grade is E"
    | n < 50 = "Grade is D"
    | n < 60 = "Grade is C"
    | n < 80 = "Grade is B"
    | otherwise = "Grade is A"
  |]

mkTriple :: Q Exp
mkTriple = [| \x -> x * x * x |]


