Forth-nonsense
==============

Attempts to embed Forth in Haskell.

This is just a trivial little example of how you could embed a simple postfix language into Haskell by using the existing do-notation.

Currently I only support pushing numbers to the stack and adding them.

You can write code like:

    res = runProgram $ do {1; 2; 3; add; add; end}
    
this should give you `Right 6`.

You can even define and use your own "words":

    incr = do {1; add}
    res = runProgram $ do {1; 2; 3; add; add; incr; incr; end}
    
this should give you `Right 8`.

If you make an error, you would get something like `Left StackUnderflow`.