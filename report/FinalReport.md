<h1 align="center">Final Report</h1>

This report is for the software testing homework.
For more information and latest update, please visit the project homepage,
all the code and the report is available there:
https://github.com/li-zhirui/JSAnalyzer


<p align="right">Author: Li Zhirui</p>

<h2 id="abs">Abstract</h2>

This project is an implementation of an analyzer for a
JavaScript-like language, referring to JavaScript 1.4 grammar.
I use Haskell to implement the backend of analyzer.
I first read the source file and parse the code into abstract syntax tree,
and then I tried to evaluate the expression and analyze the program
by simulate running process.

What have been done now is shown as below:

- Find out which functions that have been defined are never been called.
- Find out which statements have not reached.
- Print out the result in console.

TODO List:

- Write a front-end page to show the result.
- Analyze which condition branch is not covered.
- Support more JavaScript features, especially the object-oriented part

<h2 id="con">Contents</h2>

- [Abstract](#abs)
- [Contents](#con)
- [Techniques](#tech)
  - [Syntax](#tech-syntax)
  - [Haskell](#tech-haskell)
  - [Megaparsec](#tech-megaparsec)
- [Implementation](#impl)
  - [Overview](#impl-overview)
  - [Entity](#impl-entity)
  - [Parsing](#impl-parse)
  - [Analyzing](#impl-analyze)
  - [Error Handling](#impl-error)
- [Execution](#exe)
- [Testing](#test)
- [Summary & Further Works](#sum)
- [References](#ref)

<h2 id="tech">Techniques</h2>

<h3 id="tech-syntax">Syntax</h3>

The language that is being analyzed is a JavaScript-like language,
not JavaScript itself. It does not support many features of JavaScript,
such as prototype, try-catch block, this object, with statement,
switch statement, and so on.
Because time is too limited for me to implement all syntax of JavaScript,
and it is not the essence of this project.
What features I have kept are as follows:

- if-else statement
- for-loop
- control-flow key words, such as break, continue, return
- first-class function, including anonymous function
- dynamic-typing
- weak-typing
- array literal
- object literal

It is well known that JavaScript is not a well-designed language,
so I modify some of them.
The language I have implemented supports lexical closure,
and the scope of variable is strict,
you can't access a variable outside a function
that is defined in the function.
And defining multiple variables of the same name in one scope
will cause a name conflict error.

<h3 id="tech-haskell">Haskell</h3>

The backend is implemented using Haskell.
Haskell is a standardized, general-purpose purely functional programming
language, with non-strict semantics and strong static typing.
It is named after logician Haskell Curry.
The latest standard of Haskell is Haskell 2010. As of February 2016,
a group is working on the next version, Haskell 2014.
The reason I use Haskell is that Haskell has a concise syntax.
And the way that it solve this problem is really elegant.
The other reason is that I am learning this language recently,
and this project can be treated as a kata.

<h3 id="tech-megaparsec">Megaparsec</h3>

Megaparsec is a fork of Parsec library.
Parsec is an industrial strength, monadic parser combinator library for Haskell.
It can parse context-sensitive, infinite look-ahead grammars
but it performs best on predictive (LL[1]) grammars.

Parsing source code using Megaparsec is exactly similar to writing BNF for a
specific language. For example, if I want to parse a boolean value
representing by string true or false,
this can be represented in BNF as:

```bnf
<boolean> ::= "true" | "false"
```

using Megaparsec, the Haskell code will look like:

```haskell
pBoolString :: Parser String
pBoolString  = string "true" <|> string "false"
```

almost the same except some symbols.

To show how awesome Parsec is,
here I list some clones of Parsec in other languages:

- PCL for OCaml
- JParsec for Java
- NParsec, JParsec ported to C#
- Ruby Parsec, JParsec ported to Ruby
- FParsec for F#
- ...

<h2 id="impl">Implementation</h2>

<h3 id="impl-overview">Overview</h3>

The implementation contains 3 main parts, the first part is the entity that can
be used by other parts of the analyzer, the others are corresponding to each
steps of analyze. In addition to this 3 main parts, I will also introduce the
error handling strategy.

First, parse the source code string into a abstract syntax tree. The result of
the first step will be the input of the analyzer. The analyzer contains the
evaluator part and the analyzer part.
For expression, the evaluator will evaluate it and produce a value.
For other statements and expressions of function call,
the analyzer part will work for analyze them.

<h3 id="impl-entity">Entity</h3>

All entities are declared in `JSEntity.hs`, including `JSVal`, `JSExpression`,
`JSStatement`, `JSEnv`.

Any `JSExpression` will finally evaluated into a `JSVal`, a `JSVal` may be
one of boolean, number, string, closure, array, objcet, atom,
null, undefined, this.
A number may be a int or double or NaN(not a number).
A closure contains a function and the environment where the function is defined.
That simulate the lexical closure.
An atom is an identifier, that's the name of any `JSVal`, such numbers,
functions, and so on.

The data definition is clear:

```haskell
data JSVal =
             JSBool Bool
           | JSInt Integer
           | JSFloat Double
           | JSString String
           | JSFunction { params           :: [String]
                        , functionBody     :: [JSStatement]
                        , functionBeginPos :: (Int, Int)
                        , functionEndPos   :: (Int, Int)
                        }
           | JSClosure JSVal JSEnv
           | JSArray [JSVal]
           | JSObject [(String, JSVal)]
           | JSAtom String
           | JSNull
           | JSUndefined
           | JSThis
           | JSNaN
```

The definition of `JSExpression` and `JSStatement` are just like `JSVal`,
for saving page spaces, I won't write them here, please read the source code.
What I want to mention is that, the `JSStatement` contains not only the
statement itself, but also contains the position of where it is definition:

```haskell
data JSStatement =
  JSStatement { statementContnet  :: JSStatementContent
              , statementPosBegin :: (Int, Int)
              , statementPosEnd   :: (Int, Int)
              }
```

This is because I need the position information for analyzing the code, and
producing distinct result.

Come to the `JSEnv`, this represents the environment where the expression
being evaluated. It contains a `JSScope` stack, which is a list of key-value
pairs recording variables and functions can be get in that scope.
The scope stack is used to represent hierarchical variables acquiring.
And it is useful for constructing closure.

Because Haskell is a pure functional language, there is no concept of variable,
so I need some way to simulate it. `IORef` is the way I choose.
All side effects are encapsulated in the `IO` monad,
so that I can use do-notation to simulate imperative way of modifying them.
For example, here is the way I set variable's value in a scope:

```haskell
setVarInScope :: JSScope -> String -> JSVal -> IOThrowsError JSVal
setVarInScope scopeRef var value =
  do scope <- liftIO $ readIORef scopeRef
     maybe (throwError $ UnboundVar "Try to set an unbound variable" var)
           (liftIO . (`writeIORef` value))
           (lookup var scope)
     return value
```

That's almost the same way as in imperative language like Java.

<h3 id="impl-parse">Parsing</h3>

Though the Megaparsec can parse text in a concise and clear way,
it is not very powerful. It does not support back tracking by default,
and it can't solve the situation of left recursion.

For example, if the whole parser contains the part of a function parser and
an identifier parser. Then if it come across a string of "function0",
it should parse it as an identifier. But if I write code as follow,
the parse will fail:

```haskell
pFuncOrId :: Parser JSVal
pFuncOrId = pFunction <|> pIdentifier
```

Because the `pFunction` will eat the string "function", and the `pIdentifier`
will only see the "0". A number can't be a identifier, so the parse fails.

The way to solve this problem is to use `try` function to declare that I need
the back tracking explicitly:

```haskell
pFuncOrId :: Parser JSVal
pFuncOrId = try pFunction <|> pIdentifier
```

For an example of left recursion, let's think of a situation that
there is a function `f1` defining in JS file,
and the `f1` returns another function `f2`,
when I want to write a function that calling `f2`, I may write like this:

```javascript
x = f1(1, 2)(3);
```

That means a function call expression can be a expression followed
by parameters surrounded by parentheses:

```bnf
<expression> ::= ... | <functionCall> | ...
<functionCall> ::= <expression> <openParen> <parameters> <closeParen>
```

It can be translate to Haskell code like this:

```haskell
pExpression :: Parser JSExpression
pExpression = ... <|> pFunctionCall <|> ...

pFunctionCall :: Parser JSExpression
pFunctionCall = do func <- pExpression
                   args <- pArguments
                   return $ JSFunctionCall func args
```

However, if I write code like what I showed above,
the program won't terminate because of infinite recursion.
So, I need to do some work to transform it to LL grammar.
The way to do this is to look ahead a symbol after I parse a expression.
If the symbol is one of '(', '[' or '{', then I need to treat them
as a whole expression. Otherwise the expression can be return.
See `furtherExpression` function in `JSParser.hs` file for more details.

<h3 id="impl-analyze">Analyzing</h3>

I split the analyzing part into an evaluator and an analyzer.
The evaluator focus on evaluate expressions, while the analyzer focus on
analyze and run the statements.

#### Evaluator

The evaluator located in `JSEvaluator.hs` file.
All expression should be finally evaluate to a value,
and that is the duty of the evaluator.

JavaScript is a dynamic-typing and week-typing language,
on the contrary, Haskell is a static-typing and strong-typing language.
So I need to deal with the conflict.

First, JavaScript treat any value except for
boolean false, undefined, null, NaN, 0, and empty string as boolean true,
that's why I need the function `asJSBool`:

```haskell
asJSBool :: JSVal -> JSVal
asJSBool = JSBool . asBool
  where
    asBool :: JSVal -> Bool
    asBool JSUndefined   = False
    asBool JSNull        = False
    asBool JSNaN         = False
    asBool (JSInt 0)     = False
    asBool (JSFloat 0)   = False
    asBool (JSString "") = False
    asBool (JSBool b)    = b
    asBool _             = True
```

Similarly, there are functions called `asJSNumber` and `asJSString` to
convert a value to number and string.

The calculative strategy in JavaScript is strange,
so it needs a lot of code to work with this.
For examples:

```javascript
5 + 5;              // 10
5 + "5";            // "55"
5 - 5;              // 0
5 - "5";            // 0
NaN == NaN;         // false
```

For implementation details, please see the code in `JSEvaluator.hs`,
for more about calculative strategy of JavaScript,
you could visit the W3School online lessons.

Generally speaking, the evaluate the return value of a function call
should also be the duty of evaluator.
However, a function contains many statements in its body,
and the process of running each statements should be analyzed by analyzer,
so the function call will be evaluated in `JSAnalyzer.hs`

#### Analyzer

The analyzer first traverse the whole program to find out all the
function declarations and statements,
and record all of these information in `ProgramFlowRecord`.

```haskell
type FunctionCallCount = (JSVal, IORef Int)
type StatementRunCount = (JSStatement, IORef Int)
type ProgramFlowRecord = ([FunctionCallCount], [StatementRunCount])
```

All the count will be initialized to 0.
Then the analyzer will simulate running the whole program.
For every statement it encounter,
the analyzer updates the corresponding count in the record.
And for every function call,
the corresponding function call count will be updated.

```haskell
analyzeStatement :: ProgramFlowRecord -> JSEnv
                 -> JSStatement -> IOThrowsError StatementType
analyzeStatement record@(_, sRunCounts) env stmt =
  do countRef <- maybe (liftIO (newIORef 0)) return (lookup stmt sRunCounts)
     count    <- liftIO $ readIORef countRef
     _        <- liftIO $ writeIORef countRef (count + 1)
     analyzeContent record env (statementContnet stmt)
```

As I mention before, the evaluation of function is done by analyzer.
Functions in JavaScript is not like functions in C,
where functions are only pointers.
And they behaves not like functions in Emacs Lisp,
which use dynamic-scope as strategy.
To simulate the lexical closure,
we should evaluate the function in the environment where it has been defined.
So when the evaluator find a function declaration,
it will encapsulate the function and the environment together
to build a closure.
A `JSClosure` is also a `JSVal`.
The parameter of `analyzeFunctionCall` function is never a row function,
but a closure containing both the function and the environment.
When calling a function,
the arguments are evaluated in the environment where the function is called.
The result values will be named by the formal parameters, then they will
insert to the head of the environment where the function was defined,
and this new environment will be the environment where the function will run.

```haskell
analyzeFunctionCall :: ProgramFlowRecord -> JSEnv -> JSVal
                    -> [JSExpression] -> IOThrowsError JSVal
analyzeFunctionCall record@(fCount, _) env (JSClosure func funcEnv) argExprs =
  do ...
     let analyzeExpr = analyzeExpression record env
     args     <- sequence (analyzeExpr <$> argExprs)
     let paramNum = length $ params func
     if length args /= paramNum
       then throwError $ NumArgs (toInteger paramNum) args
       else
         do scope <- liftIO . initScope $ zip (params func) args
            sType <- analyzeStatements record (scope:funcEnv) (functionBody func)
            case sType of
              ReturnStatement _ val -> return val
              NormalStatement _     -> return JSUndefined
              _ -> throwError $ invalidStatementType sType
```

<h3 id="impl-error">Error Handling</h3>

Error handling is a little tricky in Haskell, because there is no try-catch
block as in some normal languages like Java, C++.
The way to handle errors in this analyzer is use `Control.Monad.Except`
and `Either` (`Either` is also an instance of `Except`).

The error data type is defined in `JSError.hs` file.

The `parseString` function defined in `JSParser.hs` file has type
`Parser a -> String -> ThrowsError a`, which means it accept a parser for
type a string, it tried to parse the string using the parser, if the parsing
succeed, it will encapsulate the result using `Right` constructor, otherwise
a `JSError` value will be return in `Left`.
The `ThrowsError` is a type alias defining as:

```haskell
type ThrowsError = Either JSError
```

Similarly, the way throwing error in evaluator and analyzer is using
`IOThrowsError`, because they need to do with IO operation.

<h2 id="exe">Execution</h2>

To run the code, you need to install cabal and GHC, type the following
command in console:

```
> cabal run
```

There will be a prompt message:

```
"Please input JS file path:"
```

Input the file path and press enter,
you can see the result produced by the analyzer.

For example, if you analyze a JavaScript code snippet like this:  

```javascript
function f1() {
  var x = 3;
  if (x > 2) {
    return x + 1;
  } else {
    return x - 1;
  }
  return "unreachable!";
}
function f2() {
  function f3() { }
  return "uncalled!";
}
for (var x = 1; x < 5; x++) {
  f1();
}
var arr = [function(a) {}, function(b) {}, 1, 2]
arr[0](1)
```

The result will be print on the console:

```
function never been called at line: 10 -- function(){function f3(){};return uncalled!;}
function never been called at line: 11 -- function(){}
function never been called at line: 17 -- function(b){}
can't reach statement at line: 5 -- {return (x-1);}
can't reach statement at line: 6 -- return (x-1)
can't reach statement at line: 8 -- return unreachable!
can't reach statement at line: 11 -- function f3(){}
can't reach statement at line: 12 -- return uncalled!
```

<h2 id="test">Testing</h2>

I test the code using the DocTest package of Haskell. DocTest is a small
program, that checks examples in Haddock comments. It is similar to the
popular Python module with the same name.

Here is an example that use DocTest as the unit test tool in `JSAnalyzer.hs`,
where the comments following the `Examples:` are testing code:

```haskell
-- | concat corresponding list in two pairs of lists
--
-- Examples:
-- >>> concatPair ([], []) ([], [])
-- ([],[])
-- >>> concatPair ([1], [2]) ([3], [4])
-- ([1,3],[2,4])
concatPair :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
concatPair (as, bs) (as', bs') = (as `mappend` as', bs `mappend` bs')
```

To use the package, you need to install it first:

```
> cabal install -g doctest
```

Then you can you can use DocTest to test files.

```
> doctest Main.hs
```

Then you will get the result output:

```
Examples: 374  Tried: 374  Errors: 0  Failures: 0
```

<h2 id="sum">Summary & Further Works</h2>

In this project,
I used Haskell to parse source code of a JavaScript-like language,
and tried to analyze the code by simulating running it.
Working with Haskell is interesting, it offers a new way of thinking.
And it improve the ability of parsing strings,
which is really useful in real work.
However, the analyzer misses some important functions as I mention in the
TODO list, because the lack of time and I will improve it when I have time.

<h2 id="ref">References</h2>

1. Haskell Language <https://www.haskell.org/>
1. JavaScript 1.4 Grammar <http://www-archive.mozilla.org/js/language/grammar14.html>
1. ECMAScript® Language Specification <http://www.ecma-international.org/ecma-262/5.1/>
1. W3School JavaScript Tutorial <http://www.w3schools.com/js/default.asp>
1. megaparsec: Monadic parser combinators <https://hackage.haskell.org/package/megaparsec>
1. Switch from Parsec to Megaparsec  <https://mrkkrp.github.io/megaparsec/tutorials/switch-from-parsec-to-megaparsec.html>
1. Parsing a simple imperative language <https://mrkkrp.github.io/megaparsec/tutorials/parsing-simple-imperative-language.html>
1. The DocTest package <https://hackage.haskell.org/package/DocTest>
