-- Based on section 5 of "Programming with arrows" by John Hughes
module S5sim (
    Event(..)
  , Message
  , Sim
  , State
  , Time
  , causal
  , cutOff
  , delay
  , delayFirst
  , infinity
  , lift
  , message
  , nubA
  , printA
  , quiescent
  , ready
  , runSim
  , sim
  , wait
  , waitInput
)
where

-- `fmap' is just as good and the more modern name, but
-- the original paper uses `liftM`
import Control.Monad (liftM)
import Data.Monoid ((<>))

import Arrows (
    (>>>)
  , arr
  , first
  , Arrow
  )
import DList

type Time = Double

class Message m where
  message :: Monad m => String -> m ()

-- I don't have internet access to search from which
-- module this is exported
infinity :: Time
infinity = 1 / 0

data Event a = Event { time :: Time, value :: a }
  deriving (Eq)

instance (Show a) => Show (Event a) where
  show (Event t x) = show x ++ "@" ++ show t

newtype Sim m a b = Sim (a -> m (b, State m a b))

instance Monad m => Arrow (Sim m) where
  -- arr :: (a -> b) -> Sim (a -> m (b, State m a b))
  arr f = Sim $ \a -> return (f a, s)
    where s = waitInput $ \(Event t a) ->
                ready (Event t $ f a) s
  -- (>>>) :: Sim m a b -> Sim m b c -> Sim m a c
  (Sim f) >>> (Sim g) = Sim $ \a -> do
    (b, sf) <- f a
    (c, sg) <- g b
    return (c, sf `stateComp` sg)
  -- first :: Sim m a b -> Sim m (a, c) (b, c)
  first (Sim f) = Sim $ \(a, c) -> do
    (b, s) <- f a
    return ((b, c), stateFirst b c s)

stateFirst :: Monad m
           => b -> c -> State m a b
           -> State m (a, c) (b, c)
stateFirst b c (Wait t d k) =
    wait t
         (stateFirst b c d)
       $ \(Event t' (a', c')) ->
             ready (Event t' (b, c'))
                 $ stateFirst b c' $ k $ Event t' a'
stateFirst b c (Lift m) = lift $ liftM (stateFirst b c) m
stateFirst b c (Ready (Event t b_) s) =
    wait t
         (ready (Event t (b_, c))
              $ stateFirst b_ c s)
       $ \(Event t' (a', c')) ->
                ready (Event t' (b, c'))
                    $ stateFirst b c' $ s `after` Event t' a'

data State m a b =
    -- ready to output event, then go to next state
    Ready (Event b) (State m a b)
    -- simulation ready to perform computation in underlying
    -- monad and continue with state that it will return
  | Lift (m (State m a b))
    -- wait for event, continuing with state in the case of
    -- a timeout, otherwise use the event to compute the
    -- next state
  | Wait Time (State m a b) (Event a -> State m a b)

delayFirst :: (Monad m, Eq a)
           => Time -> Sim m (a, b) (a, b)
delayFirst dt = sim $ \(a, b) -> return ((a, b), delayState a)
  where enqueue a evs ev@(Event t (v, w)) =
          ready (Event t (a, w))
              $ wait (t + dt)
                     (ready (Event (t + dt) (v, w))
                          $ case dlDeconstruct evs of
                              Nothing -> delayState v
                              Just (Event t' v', evs') ->
                                  enqueue v evs' $ Event t' (v', w))
                   $ enqueue a (evs <> (dList1 $ Event t v))
        delayState a =
          waitInput $ \ev@(Event t (v, w)) ->
            if a == v
            then ready ev $ delayState a
            else enqueue a dlNull ev

delay :: (Monad m, Eq a)
      => Time -> Sim m a b -> Sim m a b
delay delay (Sim f) = Sim $ \a -> do
    (b, s) <- f a
    return (b, loop delay s)
  where loop delay (Ready e@(Event t v) s) =
          ready (Event (delay + t) v) $ loop delay s
        loop delay (Lift m) =
          lift $ liftM (loop delay) m
        loop delay (Wait t dflt k) =
          wait t (loop delay dflt) $ loop delay . k

nubA :: (Monad m, Eq a) => Sim m a a
nubA = sim $ \a -> return (a, nubState a)
  where nubState a = waitInput $ \ev@(Event _ v) ->
                       if a == v
                       then nubState a
                       else ready ev $ nubState v

stateComp :: Monad m
          => State m a i -> State m i b -> State m a b
sf `stateComp` (Ready b sg) = ready b $ sf `stateComp` sg
sf `stateComp` (Lift sg) = lift $ liftM (sf `stateComp`) sg
(Ready i sf) `stateComp` sg@(Wait t dflt k)
  | time i < t = sf `stateComp` k i
  | otherwise  = sf `stateComp` dflt
(Lift sf) `stateComp` sg@(Wait _ _ _) =
                 lift $ sf >>= return . (`stateComp` sg)
sf@(Wait tf df kf) `stateComp` sg@(Wait tg dg kg) =
    wait (min tf tg)
         timeout
       $ \a -> kf a `stateComp` sg
  where timeout | tf < tg  = df `stateComp` sg
                | tf == tg = df `stateComp` dg
                | tf > tg  = sf `stateComp` dg

after :: Monad m
      => State m a b -> Event a -> State m a b
after (Ready b state) event = Ready b $ state `after` event
after (Lift state) event = lift $ liftM (`after` event)
                                        state
after z@(Wait t deflt f) event
  | time event < t = f event
  | otherwise      = deflt `after` event

printA :: (Show a, Message m, Monad m)
       => String -> Sim m a a
printA prefix = Sim $ \a -> do
    message $ prefix ++ ": " ++ show a ++ "@init"
    return $ (a, s)
  where s = waitInput $ \e -> lift $ do
              message $ prefix ++ ": " ++ show e
              return $ ready e s

runSim :: Monad m => Sim m a b -> a -> [Event a] -> m ()
runSim (Sim f) a as = do
  (_, r) <- f a
  runState r as

cutOff :: Monad m => Time -> Sim m a b -> Sim m a b
cutOff t (Sim f) = Sim $ \a -> do
    (b, s) <- f a
    return (b, cutOffState t s)

cutOffState :: Monad m => Time -> State m a b -> State m a b
cutOffState t (Ready ev s)
  | time ev < t = ready ev $ cutOffState t s
  | otherwise   = let z = waitInput $ const z in z
cutOffState t (Lift m) = lift $ liftM (cutOffState t) m
cutOffState t (Wait d s k)
  | d < t = Wait d (cutOffState t s) $ cutOffState t . k
  | otherwise = wait t
                     (let z = waitInput $ const z in z)
                   $ cutOffState t . k

runState :: Monad m => State m a b -> [Event a] -> m ()
runState (Ready _ s) as = runState s as
runState (Lift m) as = m >>= flip runState as
runState (Wait t s k) [] -- no further inputs
  | t == infinity = return () -- infinity never comes
  | otherwise     = runState s [] -- timeout
runState (Wait t s k) orig@(a:as)
  | t <= time a = runState s orig -- timeout
  | otherwise   = runState (k a) as -- supply event

-- partial function
quiescent (Ready _ _) = error "Partial function: state ready to output is NOT quiescent"
quiescent (Lift m) = Lift $ liftM quiescent m
quiescent (Wait t s k) = Wait t (quiescent s) k

-- partial function
causal t z@(Ready e f)
  | t <= time e = z
  | otherwise   = error "Anti causal"
causal t (Lift m) = Lift $ liftM (causal t) m
causal t (Wait t' s k) = Wait t' (causal t s) (causal t . k)

-- smart constructors
sim f = Sim $ \a -> do
    (b, s) <- f a
    return (b, quiescent s)

ready e s = Ready e $ causal (time e) s
lift = Lift
wait t f k = Wait t (causal t f) $ \e ->
               causal (time e) (k e)

waitInput :: (Event a -> State m a b) -> State m a b
waitInput = Wait infinity $ error "Waiting for infinity timed out"
