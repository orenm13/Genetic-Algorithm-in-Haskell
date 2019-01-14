module Formulas where

import           Numeric.LinearAlgebra
import           Control.Monad.IO.Class
import           Control.Monad (replicateM)
import           System.Random
import qualified Data.Vector as V
import           Data.List

type X          = Matrix R
type Beta       = Vector R
type Y          = Vector R
type Block      = V.Vector Int
type Powers     = V.Vector Block
type Input      = V.Vector Double
type Inplist    = V.Vector Input
type Polynomial = (Beta, X)


-- V.Vectors, Vectors and Matrices Transformators
vvToM :: V.Vector (V.Vector Double)
      -> Matrix Double
vvToM = fromLists . V.toList . V.map V.toList

mToVV :: Matrix Double
      -> V.Vector (V.Vector Double)
mToVV = V.fromList . map V.fromList . toLists
-----------------------------------------------------

-- (5) in the article --
f :: Powers
  -> Input
  -> V.Vector Double
f pss inp = V.map oneBlock pss
    where
        oneBlock ps = V.product $ V.zipWith (**) inp $ V.map fromIntegral ps

calcX :: Inplist
      -> Powers -> X
calcX inputs pss = vvToM (V.map (f pss) inputs)

-- (~7) in the article --
cost :: Beta
     -> X
     -> Y
     -> Double
cost b x y = abs $ sumElements $ e**2
    where e = y - x #> b

-- (8) in the article --
normEq :: X
       -> Y
       -> Beta
normEq x y  = (inv (tr x <> x) <> tr x) #> y :: Beta
---------------- Show polynomial ---------------------
showPolynomial :: Beta
               -> Powers
               -> String
showPolynomial coeffs powers =
    intercalate " +" $ zipWith showCoeff coeffs' powers'
        where
            coeffs' = filter isNotZero $ toList coeffs
            powers' = V.toList $ V.map V.toList powers
            showCoeff coeff powerList = " " ++ show_ (round3 coeff) ++ matchVarsPowers powerList
            isNotZero x = round3 x /= 0

matchVarsPowers :: [Int] -> String
-- Example: [1, 4, 2] => x1 * x2^4 * x3^2
matchVarsPowers powers =
    intercalate "" $ zipWith varToPower varNums powers
        where
            varNums = [1..length powers]
            varToPower :: Int -> Int -> String
            varToPower ind 0   = ""
            varToPower ind 1   = " * x" ++ show ind
            varToPower ind pow = " * x" ++ show ind ++ "^" ++ show_ pow

-- show_ :: Num n => n -> n
show_ num = if num > 0 then show num else "(" ++ show num ++ ")"

    -------------------------------------------------------
randomPowerGen :: Int      -- m
               -> Int      -- n
               -> IO (V.Vector (V.Vector Int)) -- m >< n Random Powers
randomPowerGen m n = do
    lists <- replicateM m $ replicateM n $ randomRIO (-4, 4)
    return $ V.fromList $ map V.fromList lists

round3 :: Double -> Double
round3 x = (fromInteger $ round $ x * (10^3)) / (10.0^^3)
