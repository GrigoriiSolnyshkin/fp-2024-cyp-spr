# HW11

## Deadline 23:59 30.04.2024

Use monad transformers to implement REPL for our expression language. Don't implement the transformers yourself, use `mtl` (or some other) library. You're going to need your implementation of the parser of expressions in the prefix notation and the expression evaluator here.  

The REPL commands are: 

* (3 points) `:let` that expects a name for a variable and an expression associated with it. If parsing of both the variable name and the expression associated with it succeeded, evalute the expression and store the pair in a `State`.  If there is an error, report it. 
  
  - `:let x 13` results in associating the variable `x` with `13`. 
  - `:let y (+ * x x 1)` results in associating the variable `y` with `+ * x x 1 = 170`, provided that `:let x 13` was run beforehand. 
  - `:let 123 x` results in a syntax error: `expected identifier, but got 123`. 
  - `:let x` results in a syntax error: `unexpected eof`. 
  - `:let x (+ 1)` results in a syntax error: `incorrect expression: + 1`
  - `:let x z` results in an undefined variable error, provided that `z` isn't in the environment.


* (3 points) `:eval` that expects an expression to evaluate. If expression can be evaluated, print the result. If not, report an error.

  - `:eval x` evaluates to `13`, provided `:let x 13` was run beforehand.
  - `:eval + x x` evaluates to `26`, provided `:let x 13` was run beforehand. 
  - `:eval + 1` results in syntax error. 
  - `:eval z` results in an undefined variable error, provided `z` isn't in the environment. 

* (3 points) `:env` prints out the current environment into the standard output if no argument is given. If provided with a file name, it stores the environment into the file. If there's any issues with the file, it reports an error. 

  - `:env`, provided that `:let x 13` and `:let y (+ * x x 1)` were run beforehand, results in:  
    ```
    Current environment: 
    x -> 13
    y -> 170
    ```
  - `:env env.txt` stores the environment in the `env.txt` file, provided you have the access to it. 

[Lecture on monad transformers](https://www.youtube.com/watch?v=IkTBsLPdMxU&list=PLQsQ42jQ8PJGP0o6DvSoLzx65s3H14JF2&index=21)

[Real World Haskell on monad transformers](https://book.realworldhaskell.org/read/monad-transformers.html)

[mtl package](https://hackage.haskell.org/package/mtl)

[Real World Haskell on IO](https://book.realworldhaskell.org/read/io.html) -- see the chapter Working With Files and Handles.