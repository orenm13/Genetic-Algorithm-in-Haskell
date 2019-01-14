{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Polynomials where

import           Numeric.LinearAlgebra
import           System.Random hiding (rand)
import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Exception
import           Data.Maybe
import qualified Data.Vector as V
import qualified Data.Array.Accelerate as A
import           Genetic
import           Formulas

import Debug.Trace

data Article = Article
            { _articlePowers  :: Powers
            , _articleInputs  :: Inplist
            , _articleOutputs :: Y
            } deriving (Eq, Ord)

instance Show Article where
      show = show . _articlePowers

instance Individual Article where
   -- fitness :: a -> IO (Maybe Double)
      fitness (Article ps is os) = do
            let x' = calcX is ps
                rows = length $ toLists x'
                cols = length . head $ toLists x'
                y = os
            noise_ <- rand rows cols
            let   noise = noise_ * 100.0
                  x = if det (tr x' <> x') == 0
                      then x' + noise
                      else x'
            -- bug fix for BLAS matrix rank on big numbers
            return $ if det (tr x <> x) == 0
                     then Nothing
                     else Just $ cost (normEq x y) x y

   -- mutateIndividual :: a -> IO a
      mutateIndividual (Article pows _a _b) = do
            r <- randomIO
            if mutationProbability < r
            then inner pows
            else return $ Article pows _a _b
            where
                  mutationProbability :: Double
                  mutationProbability = 0.1

                  inner :: Powers -> IO Article
                  inner x = do
                        blockIndex <- randomRIO (0, V.length x - 1)
                        powerIndex <- randomRIO (0, V.length (x V.! 0) - 1)
                        newPower   <- randomRIO (-4, 4)

                        let updated = updateVV x ( blockIndex, powerIndex) newPower
                        return $ Article updated _a _b


   -- crossover :: (a, a) -> IO a
      crossover ( Article pows1 _a _b
                , Article pows2 _ _
                ) = do
            index <- randomRIO (0, V.length pows1)
            let leftPart  = V.take index pows1
            let rightPart = V.drop index pows2
            return $ Article (V.concat [leftPart, rightPart]) _a _b


updateVV :: V.Vector (V.Vector a) -- what to update
         -> (Int, Int)            -- where
         -> a                     -- what to put there
         -> V.Vector (V.Vector a) -- result
updateVV vvs (xIdx, yIdx) value =
      vvs V.// [(xIdx, updatedV)]
      where
            updatedV  = originalV V.// [(yIdx, value)]
            originalV = vvs V.! xIdx