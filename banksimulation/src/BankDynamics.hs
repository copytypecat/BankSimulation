module BankDynamics where 

import Control.Monad

import DataAggregation

-- elementary data types

-- type synonym to distinguish time
type Time = Double

-- data type to keep track of temporal values for each customer 
-- "closing" is the chosen term to indicate the completion of their teller service
data Person = Person { waitTime     :: Time
                     , untilArrival :: Time
                     , untilClosing :: Time
                     } deriving (Eq, Show)

-- register wait time  
wait :: Time -> Person -> Person
wait t (Person w a c) = Person (w + t) a c

-- shift the clock to indicate the approach of events
arrivalNearing :: Time -> Person -> Person
arrivalNearing t (Person w a c) = Person w (a - t) c

closingNearing :: Time -> Person -> Person
closingNearing t (Person w a c) = Person w a (c - t)

type People = [Person]

-- data type to keep track of the people in the various stages of the bank pipeline
-- along with a bank clock as a source of "universal time"
data Bank = Bank { closed  :: People
                 , serving :: Maybe Person
                 , queue   :: People
                 , future  :: People
                 , clock   :: Time
                 } deriving (Eq, Show)

-- we model a simulation as the history of the bank's states at each critical event
type Simulation = [Bank]

-- given lists of arrival and service times, generate the Persons that will come to the bank
populate :: [Time] -> [Time] -> Bank
populate arrivalTimes closingTimes = Bank [] Nothing [] freshFuture 0 
    where freshFuture = zipWith (Person 0) arrivalTimes closingTimes

-- helper operations
(>+) :: [a] -> a -> [a]
(>+) xs x = xs ++ [x]

(<$:>) :: (a -> a) -> [a] -> [a]
(<$:>) f []     = []
(<$:>) f (x:xs) = (f x:xs)
    
-- state dynamics

-- two types of critical state-changing events
data Transit = Arrival | Closing

-- compute which event occurs next given the soonest ticking clock
nextTransit :: Bank -> Maybe Transit
nextTransit (Bank _ x _ fs _) = case (x, fs) of
    (Just x , (f:_)) -> if untilArrival f <= untilClosing x
                        then Just Arrival
                        else Just Closing
    (Nothing, (f:_)) -> Just Arrival
    (Just x , []   ) -> Just Closing
    (Nothing, []   ) -> Nothing

-- enact the state transition corresponding to the arrival of a new customer to the bank
-- we first compute the time delta and apply it to update the clock and the people
-- we then update the pipeline depending on the state of the teller
-- case I: if the teller is unoccupied (and hence the queue is empty), the new arrival gets served
-- case II: if the teller is occupied, the new customer goes to the end of the queue
arrival :: Bank -> Bank
arrival (Bank cs x qs (f:fs) u) = let 
    t   = untilArrival f 
    u'  = u + t
    f'  = arrivalNearing t f
    qs' = wait t <$> qs
    x'  = closingNearing t <$> x
    in case x' of
        Nothing -> Bank cs (Just f')  qs'        fs u'
        Just _  -> Bank cs x'        (qs' >+ f') fs u'

-- enact the state transition corresponding to the closing of a customer's service
-- we first compute the time delta and apply it to update the clock and the people
-- we then update the pipeline depending on the state of the queue
-- case I: if the queue is empty, the teller is left unoccupied (but the next arrival is sooner)
-- case II: if the queue is nonempty, its head person begins their teller service
closing :: Bank -> Bank
closing (Bank cs (Just x) qss fs u) = let 
    t    = untilClosing x
    u'   = u + t
    fs'  = arrivalNearing t <$:> fs
    qss' = wait t <$> qss
    x'   = closingNearing t x
    in case qss' of
        []     -> Bank (cs >+ x') Nothing   []  fs' u'
        q':qs' -> Bank (cs >+ x') (Just q') qs' fs' u'

-- run the bank simulation until all customers are served
simulate :: Bank -> Simulation
simulate bank = bank : future where  
    future = case nextTransit bank of
        Just Arrival -> simulate $ arrival bank
        Just Closing -> simulate $ closing bank 
        Nothing      -> []

-- simulation data extraction

-- extract the final stage completed customers' wait times
waitTimes :: Simulation -> [Time]
waitTimes = (waitTime <$>) . closed . head . reverse

averageWait :: Simulation -> Time
averageWait = average . waitTimes

maximumWait :: Simulation -> Time 
maximumWait = maximum . waitTimes

data WaitReport = WaitReport {avgWait :: Time, maxWait :: Time} deriving Show

getWaitReport :: Simulation -> WaitReport
getWaitReport = liftM2 WaitReport averageWait maximumWait

-- the wait radius is the difference between the maximum and average wait times
getWaitRadius :: Simulation -> Double
getWaitRadius = (liftM2 (-) maxWait avgWait) . getWaitReport

queueLength :: Bank -> Double
queueLength = fromIntegral . length . queue

-- return the history of the queue as a graph (see: DataAggregation) 
queueHistory :: Simulation -> Graph
queueHistory = (clockQL <$>)
    where clockQL = liftM2 TimeVal clock queueLength

averageQueue :: Simulation -> Double
averageQueue = averageFn . queueHistory

maximumQueue :: Simulation -> Double 
maximumQueue = maximumFn . queueHistory

data QueueReport = QueueReport {avgQueue :: Time, maxQueue :: Time} deriving Show 

getQueueReport :: Simulation -> QueueReport
getQueueReport =  liftM2 QueueReport averageQueue maximumQueue