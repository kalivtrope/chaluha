# Chaluha
Chaluha is an AST-based Haskell interpreter for a small subset of lua.

## Usage
To build, simply run

```
cabal build
```

To interpret a lua script, do

```
cabal run . < ./path/to/script
```

The program currently only expects input on STDIN.

## Parsing
Chaluha contains a toy [parser](./src/Parser.hs) built on top of `Applicative` and `Alternative`.
### Parser features and issues
The parser is deterministic (i.e. always returns a single value, or an error),
but the implementation is pretty naive and backtracks a lot.

Due to the fact that the parser is unable to "commit"
to a path once it parses specific tokens,
it doesn't give a meaningful error message when fed with unparsable program.

There might also be whitespace parsing issues.

Creating a parser from scratch was done mainly for educational purposes and
I realize that in a serious implementation I should probably use an existing library such as 
(mega)parsec.

### Output
The output of the parser is an AST representing the lua program - a single *block*.

### Supported syntax
- basic assignment
    - either *local* (explicitly assigns in the current scope) or *non-local* (either reassigns a variable in the closest scope or performs a global assignment)
    - only simple identifier names are allowed on the left-hand-side (LHS) of the assignment
- function calls
    - currently the calls cannot be nested, i.e. `foo(bar)` is allowed while `foo(bar)(baz)` isn't
- conditions
- nested blocks (`do`)

See [Examples](./Examples) for example scripts.

### Unsupported syntax (and the corresponding language features)
- loops & control statements (`break`, conditional `return`s inside of a function)
- multiline strings
- string escape sequences
- multiline comments
- `goto`s and labels
- varargs expressions (`...`) and variadic functions (functions with a variable amount of parameters)
- tables

## Data representation
A block consists of series of *statements*.
The AST representation features these types of statements:

```haskell
data Statement
  = Assignment [Lhs] [Expr]
  | Call Identifier [Expr]  -- function call
  | Break
  | Return [Expr]
  | Do Block
  | If Expr Block [(Expr, Block)] (Maybe Block)
  | Local [Identifier] (Maybe [Expr])   -- local assignment
  | Dummy   -- e.g. a semicolon
  deriving (Eq)
```

Then there are *expressions*, which are expected to evaluate to a list of *values*
(for most of the cases this list will
contain just a single element, but this is
done due to the function's return values being tuples).

```haskell
data Expr
  = EValue Value
  | EBinOp BinOp Expr Expr
  | EUnOp UnOp Expr
  | EVar Identifier
  | ECall Identifier [Expr]
  | EPar Expr
  | EFuncDef [Identifier] Block
  deriving (Eq)
```

Note that a function call can be either a statement
(in which case the call's return value is simply discarded) or an expression.
Lua's grammar is unambiguous so it's always clear whether a call
is an expression rather than a statement.

```haskell
data Value
  = Nil
  | Number Numeric
  | Boolean Bool
  | String String
  | Function
      { getEnv :: Env
      , getParams :: [Identifier]
      , getBody :: Block
      }
  | Builtin BuiltinCall
  deriving (Eq)
```

The `Builtin` currently features a single value: the `print` function.

## Evaluation
The resulting AST can be evaluated using the function `runEval` from the [Eval](./src/Eval.hs) module. See [Main](./src/Main.hs) for usage.

The core of the evaluation is the `Env`ironment data structure defined in [LuaTypes](./src/LuaTypes.hs#L298-L301):

```haskell
data Env = Env
  { bindings :: Map.Map String (IORef Value)
  , parent :: Maybe Env
  , globals :: IORef (Map.Map String Value)
  } deriving (Eq)
```

The `Env` is contained in a `StateT` monad `Eval`:

```haskell
type Eval a = StateT Env (ExceptT LuaError IO) a
```

### Credits
The implementation is based on [this post](https://www.micahcantor.com/blog/about-that-reader-trick/)
and the [corresponding code](https://github.com/micahcantor/write-you-a-lisp/).
The semantics of local and global variables are a bit different in lua (compared to Scheme),
most notable difference being that global variables can be declared inside of any scope.

## Unfinished parts
To support arbitrary `return`s and loops (along with `break`s), I tried writing
the program in continuation-passing-style. I don't think I've quite grasped the concept though,
but I've at least left the partial implementation inside for potential completion.
