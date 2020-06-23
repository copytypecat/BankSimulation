module RandomGeneration where

import System.Random.MWC
import System.Random.MWC.Distributions
import Control.Monad

-- randomly generate arrival times via the uniform distribution with parameter 1/100 = 0.01
genArrivalTimes :: Int -> IO [Double]
genArrivalTimes n = do
    gen <- create
    xs <- replicateM n $ exponential 0.01 gen
    return xs

-- package alpha and beta parameters into a customer-type data type 
data Customer = Customer Double Double

-- our customer types 
yellow :: Customer
yellow = Customer 2 5

red :: Customer 
red = Customer 2 2 

blue :: Customer 
blue = Customer 5 1

customers :: [Customer]
customers = [yellow, red, blue]

-- scale a value in the interval via the beta function
scaleServiceTime :: Double -> Customer -> Double -> Double
scaleServiceTime p (Customer a b) x = p * x ** (a - 1) * (1 - x) ** (b - 1) 

scaleServiceTime' :: Customer -> Double -> Double
scaleServiceTime' = scaleServiceTime 200

-- generate uniform random values in the interval 
genRands:: Int -> IO [Double]
genRands n = do
    gen <- create
    xs <- replicateM n $ uniform gen
    return xs

-- given a customer type, randomly generate service times
genServiceTimes :: Int -> Customer -> IO [Double]
genServiceTimes n customer = (scaleServiceTime' customer <$>) <$> genRands n

