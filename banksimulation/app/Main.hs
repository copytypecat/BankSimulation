module Main where 

import RandomGeneration
import BankDynamics
import DataAggregation


main :: IO ()
main = let 
    colors = ["yellow", "red", "blue"] 
    n = 1000
    in do
        arrivalTimes <- genArrivalTimes n
        serviceTimes <- sequence $ genServiceTimes n <$> customers

        let banks = populate arrivalTimes <$> serviceTimes
        let simulations = simulate <$> banks

        let waitTimes = getWaitReport <$> simulations
        let queueLengths = getQueueReport <$> simulations
        let waitRadii = getWaitRadius <$> simulations

        print "Given only yellow customers, the average and maximum customer waiting times are:"
        print $ waitTimes!!0
        print "Given only red customers, the average and maximum queue lengths in-front of the teller are:"
        print $ queueLengths!!1
        print "The type of customer that gives the gives the closest value between the average and maximum customer waiting times is:"
        let (customerType, _) = argMin snd (zip colors waitRadii)
        print customerType