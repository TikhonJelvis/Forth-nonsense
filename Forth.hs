{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{- | This is just an attempt to learn about the free monad by
embedding a postfix language in Haskell. You can essentially implement
a forth-like language using something akin to continuation-passing
style. My impression is that the free monad is essentially isomorphic
to Cont, so I'm hoping that I can get something reasonable here. -}
module Forth where

import Data.Functor ((<$>))

  -- This is normally defined in Control.Monad.Free but is here for clarity
data Free f r = Free (f (Free f r)) | Pure r

  -- This could be written in terms of the Monad instance
instance Functor f => Functor (Free f) where
  fmap f (Pure x) = Pure $ f x
  fmap f (Free x) = Free $ (f <$>) <$> x

  -- The free monad over some functor f, somewhat unsurprisingly, is a monad. For free.
instance Functor f => Monad (Free f) where
  return = Pure
  Free x >>= f = Free $ (>>= f) <$> x
  Pure x >>= f = f x

liftF :: Functor f => f a -> Free f a
liftF functor = Free $ Pure <$> functor

-- | The basic forth commands I will support. This should be enough
-- for a proof-of-concept.
data ForthValue next = Push Int next -- ^ Push a number to the stack.
                     | Add      next -- ^ Add two numbers off the stack.
                     | End           -- ^ End the program. 
                     deriving Show

  -- This could be derived with the DeriveFunctor extension
instance Functor ForthValue where
  fmap f (Push v n) = Push v $ f n
  fmap f (Add n)    = Add $ f n
  fmap _ End        = End

-- | The free monad over the ForthValue type represents a forth program or subroutine.
type Forth = Free ForthValue
-- | A full forth program should end in () (which is the type of the
-- end function), so this is what we will actually call a ForthProgram.
type ForthProgram = Forth ()

-- | Pushes an integer to the stack. I only support ints on the stack at the moment.
push :: Int -> Forth a
push n = liftF $ Push n undefined

-- | Consume two numbers from the stack and push their sum.
add :: Forth a
add = liftF $ Add undefined

-- | End a forth program; everything after the first end will be ignored.
end :: Forth ()
end = liftF $ End

-- | Pretty print a program in the free monad over the ForthValue type
-- (that is, the type Forth).
showProgram :: Show n => Forth n -> String
showProgram (Free (Push v n)) = "push " ++ show v ++ " " ++ showProgram n
showProgram (Free (Add n))    = "add " ++ showProgram n
showProgram (Free End)        = ""
showProgram Pure{}            = error "Program not ended!"

-- | These are the different errors we can get when running a program.
data ForthError = StackUnderflow -- ^ Trying to add without at least
                                 -- two numbers on the stack.
                | StackFullEnd   -- ^ Having more than one number on the
                                 -- stack at the end of the program.
                | NotEnded       -- ^ Trying to run a program without an end.
                deriving (Show, Eq)

-- | Run a forth program of some sort. Trying to run something without
-- an end will return an error.
runProgram :: Forth n -> Either ForthError Int
runProgram program = go [] program
  where go stack (Free (Push v next)) = go (v:stack) next
        go (a:b:s) (Free (Add next))  = go (a + b : s) next
        go _ (Free Add{})             = Left StackUnderflow
        go [] (Free End)              = Left StackUnderflow
        go [result] (Free End)        = Right result
        go _ (Free End)               = Left StackFullEnd
        go _ Pure{}                   = Left NotEnded
        
  -- The rest of Num doesn't make much sense here.
instance Num (Forth a) where fromInteger = push . fromInteger

-- | This forth 'word' represents adding one to a number. 
incr :: ForthProgram
incr = do {1; add}

-- | This forth 'word' shows how you can reuse other words.
add2 :: ForthProgram
add2 = do {incr; incr}
