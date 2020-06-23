{-# LANGUAGE LambdaCase #-}

module DataAggregation where 

import Control.Monad
import Data.List (findIndex)

average :: [Double] -> Double
average = liftM2 (/) sum (fromIntegral . length)

-- a data type storing a time, value coordinate 
data TimeVal = TimeVal {time :: Double, value :: Double}

-- models the graph of a time-varying step-function as a sequence of TimeVal's
type Graph = [TimeVal] 

-- extracts a graph's sequence of intervals in terms of an interval-length, value pair
stepForm :: Graph -> [(Double, Double)]
stepForm = \case 
    TimeVal t x : xs @ (TimeVal t' _ : _) -> (t'-t, x) : stepForm xs
    _ -> []

-- integrates a graph by summing the rectangle areas subtended by the constant values over intervals
integral :: Graph -> Double
integral = sum . (uncurry (*) <$>) . stepForm

-- extracts the length of a graph's temporal domain 
interval :: Graph -> Double
interval = time . head . reverse

-- computes the average value of the graph
averageFn :: Graph -> Double
averageFn = liftM2 (/) integral interval

maximumFn :: Graph -> Double
maximumFn = maximum . (value <$>)

-- compute the term of a list that max/min-imises a given Ord-valued function
argMax :: Ord b => (a -> b) -> [a] -> a
argMax f as = let 
    bs = f <$> as
    maxVal = maximum bs
    Just maxIdx = findIndex (== maxVal) bs
    in as !! maxIdx

argMin :: Ord b => (a -> b) -> [a] -> a
argMin f as = let 
    bs = f <$> as
    minVal = minimum bs
    Just minIdx = findIndex (== minVal) bs
    in as !! minIdx