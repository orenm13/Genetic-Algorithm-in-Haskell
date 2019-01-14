{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Genetic where

import           Numeric.LinearAlgebra
import           System.Random.Shuffle    (shuffleM)
import           System.Random
import           System.ProgressBar
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Concurrent.Async (mapConcurrently)
import qualified Data.Vector as V
import           Data.List
import           Data.Maybe

class (Ord a, Eq a, Show a) => Individual a where
    fitness :: a -> IO (Maybe Double)
    mutateIndividual :: a -> IO a
    crossover :: (a, a) -> IO a

runGeneticAlgorithm :: Individual a
                    => Double      -- Goal
                    -> Int         -- maxNumIterations
                    -> Int         -- eliteSize
                    -> [a]         -- Initial (random) genetation
                    -> ProgressRef -- Progress Bar
                    -> IO a
runGeneticAlgorithm threshold
                    maxNumIterations
                    eliteSize
                    population
                    progRef   = do
    finalGroup <- foldM (makeGeneration threshold eliteSize progRef)
                        initialGeneration
                        [1..maxNumIterations]
    best finalGroup
    where
        initialGeneration = population

makeGeneration :: Individual a
               => Double      -- Goal
               -> Int         -- eliteSize
               -> ProgressRef
               -> [a]         -- population
               -> Int         -- generation number
               -> IO [a]
makeGeneration goal eliteSize progRef population _ = do
    cleanPop <- filterM (fmap isJust . fitness) population
    mFitness <- fitness =<< best cleanPop
    let bestFitness = fromMaybe (error "Invalid Fitness") mFitness
    if bestFitness < goal
    then return cleanPop
    else do incProgress progRef 1
            nextGeneration cleanPop eliteSize


nextGeneration :: Individual a
               => [a]    -- population
               -> Int    -- elite size
               -> IO [a]
nextGeneration population eliteSize = do
    children    <- makeChildren population eliteSize
    elita <- elite population eliteSize
    return (elita ++ children)

best :: Individual a
     => [a]
     -> IO a
best pop = do
    fitnesses <- mapConcurrently fitness pop
    let combined = filter (isJust . fst) $ zip fitnesses pop
    --             [(fitness, individual)]
        chosen   = minimumBy (\x y -> compare (fst x) (fst y)) combined
    return $ snd chosen

mutate :: Individual a => [a] -> IO [a]
mutate = mapConcurrently mutateIndividual

elite :: Individual a
      => [a]
      -> Int
      -> IO [a]
elite individuals nElit = do
    fitnesses <- mapConcurrently fitness individuals
    let combined   = zip fitnesses individuals
        results    = map snd . take nElit . sortOn fst $ combined
        mFitnesses = map fst . take nElit . sortOn fst $ combined
    return results

selectCouples :: Individual a
           => [a] -- population
           -> Int -- number of couples
           -> IO [(a, a)]
selectCouples population numOfCouples =
    sequence . take numOfCouples $ repeat $ selectCouple population


selectCouple :: Individual a
             => [a]
             -> IO (a, a)
selectCouple population = do
    fitnesses <- mapConcurrently fitness population
    let combined    = filter (isJust . fst) $ zip fitnesses population
        population_ = map snd . sortOn (fromJust . fst) $ combined
        indexList   = [0..(length population_ - 1)]
        duplicated  = duplicateByValue indexList
    [index1, index2] <- take 2
                      . nub
                    <$> shuffleM duplicated
    return ( population_ !! index1
           , population_ !! index2
           )

duplicateByValue :: [Int]
                 -> [Int]
duplicateByValue = foldr (\ x -> (++) (replicate x x)) []

makeChildren :: Individual a
             => [a]    -- population
             -> Int    -- nElit
             -> IO [a]
makeChildren population nElit = do
    parents   <- selectCouples population $ length population - nElit
    children  <- mapConcurrently crossover parents
    fitnesses <- mapConcurrently fitness children
    mutate $ map fst $ filter (isJust . snd) $ zip children fitnesses

bestFit :: Individual a
        => [a]
        -> IO Double
bestFit pop = minimum . catMaybes <$> mapConcurrently fitness pop
