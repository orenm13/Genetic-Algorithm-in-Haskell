module Main where

import           Numeric.LinearAlgebra
import           System.Environment (getArgs)
import           System.Random
import           System.ProgressBar
import           Control.Monad (replicateM)
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.List
import qualified Data.Vector as V
import           Polynomials
import           Genetic
import           Formulas

main :: IO ()
main = do
    (nGenerations:nPop:goal:nElit:_) <- getArgs

    ------------ Information ------------
    putStrLn ""
    putStrLn $ "No. of generatoins: "  ++ nGenerations
    putStrLn $ "Population Size: "     ++ nPop
    putStrLn $ "Score of goal: "       ++ goal
    putStrLn $ "Elite size (N-elit): " ++ nElit
    putStrLn ""

    ------------ Progress Bar -----------
    putStrLn "Progress:"
    (pb, asy) <- startProgress (msg "") (msg "") 10 (Progress 0 (read nGenerations))

    ------------ Algorithm --------------
    powers <- randomPowerGen 6 2
    let
        ------ Inps and outps of function ------
        inps  = inpsRosen
        outps = fromList $ V.toList yRosen
        ----------------------------------------
        individual = Article powers inps outps
        init = replicate (read nPop) individual

    algo <- runGeneticAlgorithm (read goal) (read nGenerations) (read nElit) init pb
    let
        finalPowers = _articlePowers algo
        x = calcX inps finalPowers
        y = outps
        coeffs = normEq x y
        ----------------------------------------
    grade <- show . fromJust <$> fitness algo

    putStrLn ""
    putStrLn $ showPolynomial coeffs finalPowers
    putStrLn $ "Fitness of final Polynomial is: " ++ grade
    putStrLn ""
    putStrLn "Attention: component with coefficient smaller tan 0.001 are rounded to nothing"


openVLists = V.map V.toList

-- Check x + 1:
simple x = x + 1.0

inpsSimple :: V.Vector Double
inpsSimple = V.fromList
       [ 1
       , 2
       , 3
       , 4]

powsSimple :: V.Vector Double
powsSimple = V.fromList
            [1.0,
             0]

ySimple = V.map simple inpsSimple

-- Check x**2 + y**2 -1:
circle [x, y] = x**2 + y**2 --  -1

inpsCircle :: Inplist
inpsCircle = V.fromList
     . map V.fromList
     $ [ [0.0001, 1.1]
       , [2, 0.001]
       , [2, 1.05]
       , [1, 2.11]
       , [0.1, 2]]

powsCircle :: Powers
powsCircle = V.fromList
          . map V.fromList
          $ [[2, 0],
             [0, 2],
             [0, 0]]

yCircle = V.map circle $ openVLists inpsCircle

-- Check Rosenbrock:
rosen [x, y] =
    1 - 2*x + x**2 + 100*y**2 + 200*x**2*y + 100*x**4

inpsRosen :: Inplist
inpsRosen = V.fromList
     . map V.fromList
     $ [[i, j] | i <- [-0.9, -0.8 .. -0.2], j <- [0.2, 0.3 .. 0.9]]


powsRosen :: Powers
powsRosen = V.fromList
        . map V.fromList
        $ [[0, 0],
           [1, 0],
           [2, 0],
           [0, 2],
           [2, 1],
           [4, 0]]

yRosen = V.map rosen $ openVLists inpsRosen
