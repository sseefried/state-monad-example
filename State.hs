module State where

import Prelude hiding (read)
import Control.Applicative

data S s a = S { runS :: s -> (a, s) } 

instance Monad (S s) where
  -- return :: a -> S a
  return a = S (\s -> (a,s))
  -- (>>=) :: S a -> (a -> S b) -> S b 
  S f >>= k = 
    -- f :: s -> (Int, s)
    S $ \s -> let (a, s') = f s 
               in runS (k a) s'
    -- k a           :: S b
    -- runS (k a)    :: s -> (b,s)
    -- runS (k a) s' :: (b, s)

read :: S s s
read = S (\s -> (s,s))

write :: s -> S s ()
write s' = S (\s -> ((), s'))

foo :: S Int Int
foo = do
  x <- read
  write (x+1)
  x <- read
  return x

test :: (Int, Int)
test = runS foo 0

{-

instance Monad (\Int -> (_,Int)) where
  -- return :: a -> (s -> (a,s))
  return a = \s -> (a,s)
  -- (>>=) :: (s -> (a,s)) -> (a -> (s -> (b,s))) -> (s -> (b,s))
  f >>= k = 
    -- f :: s -> (Int, s)
    \s -> let (a, s') = f s in k a s'

read s = (s,s)
write s' = \s -> ((), s')

foo = read >>= (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x)))

test = foo 0
     = (read >>= (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x)))) 0
     = (\s -> let (a, s') = read s 
              in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x))) a s') 0
     = let (a, s') = read 0
        in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x))) a s'
     = let (a, s') = (0, 0)
        in (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x))) a s'
     = (\x -> write (x+1) >>= (\_ -> read >>= (\x -> return x))) 0 0
     = (write (0+1) >>= (\_ -> read >>= (\x -> return x)) 0
     = ((\s -> ((), 1)) >>= (\_ -> read >>= (\x -> return x)) 0
     = (\s -> let (a, s') = (\s -> ((), 1)) s 
               in (\_ -> read >>= (\x -> return x)) a s') 0
     = let (a,s') = (\s -> ((), 1)) 0
        in (\_ -> read >>= (\x -> return x) a s')
     = let (a,s') = ((), 1)
        in (\_ -> read >>= (\x -> return x) a s')
     = (\_ -> read >>= (\x -> return x) () 1
     = (read >>= (\x -> return x)) 1

     = (\s -> let (a, s') = read s
               in (\x -> return x) a s') 1
     = let (a, s') = read 1
        in (\x -> return x) a s')
     = let (a, s') = (1,1)
        in (\x -> return x) a s')
     = (\x -> return x) 1 1
     = return 1 1
     = (\s -> (1,s)) 1
     = (1,1)

-}





























------------------------

instance Functor (S s) where
  -- fmap :: (a -> b) -> S a -> S b
  fmap f (S g) = S (\s -> let (a, s') = g s in (f a, s'))

instance Applicative (S s) where
  -- pure :: a -> S a
  pure a = S (\s -> (a,s))
  -- (<*>) :: S (a -> b) -> S a -> S b
  (S fab) <*> (S fa) = S (\s -> let (f, s')  = fab s
                                    (a, s'') = fa s'
                                 in (f a, s''))
