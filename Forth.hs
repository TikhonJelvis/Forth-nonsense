{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
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

  -- Basic forth commands:
data ForthValue next = Push Int next
                     | Add      next
                     | End   deriving Show

  -- This could be derived with the DeriveFunctor extension
instance Functor ForthValue where
  fmap f (Push v n) = Push v $ f n
  fmap f (Add n)    = Add $ f n
  fmap _ End        = End

type Forth = Free ForthValue
type ForthProgram = Forth ()

push :: Int -> Forth a
push n = liftF $ Push n undefined

add :: Forth a
add = liftF $ Add undefined

end :: Forth ()
end = liftF $ End

showProgram :: Show n => Forth n -> String
showProgram (Free (Push v n)) = "push " ++ show v ++ " " ++ showProgram n
showProgram (Free (Add n))    = "add " ++ showProgram n
showProgram (Free End)        = ""
showProgram Pure{}            = error "Program not ended!"

data ForthError = StackUnderflow
                | StackFullEnd
                | NotEnded deriving (Show, Eq)

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
