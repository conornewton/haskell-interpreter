# Simple Intepreter

## Compiling

Works with any version of ghc

```
ghc interpreter.hs parser.hs tokeniser.hs eval.hs spec.hs
```

## Running

To launch the repl, run the following after compiling

```
./interpreter
```

## Using

After launching the interpreter you will be given a repl prompt. Here you can perform basic maths operations, use variables and functions.

### Maths Stuff

Currently the evaluation of all maths operations is from left to right.

```
>2*2+1
=5
>2/2-1
=0
```

### Variables

Variable names can only contain alphabetical characters.

```
>a=2
added variable
>a
=2
>b=a*a
added variable
>b
=4
```

Variables behave slightly differently

```
>a=2
added variable
>b=a*a
added variable
>b
=4
>a=3
added variable
>b
=9
```

This occurs since b is remembered as the product of a in memory. A is only substituted for its value at evaluation.

### Functions

Functions are defined similarly to variables, the only difference is they can take parameters.
Here is an example of a function that takes two parameters.

```
>f = $0 + $1
added variable
```
To evaluate the function we can do the following

```
>f(1,3)
=4
>f(2,5)
=7
```
