module Hello where

import GailPrelude
import Prelude qualified
import Data.List qualified as List
import Distribution.Compat.Stack (callStack)
import qualified Control.Applicative as 2nd

---- Basic definitions

-- Every value has a type

-- Int is a type whose values are (bounded) integers

-- ┌─────────┐   The whole line with the :: is called a type signature
-- ┌──────────── The name we're defining
-- │   ┌──────── :: is read as "has type" or "is type"
-- │   │   ┌──── The type that we expect our definition to have
intVal :: Int -- Read this as "intVal has type Int"
intVal = 4    -- Read this as "intVal equals 4"
-- │   │ └────── The value we're giving to the identifier
-- │   └──────── = can be read as "equals" or "is" or "has the value"
-- └──────────── The name of the identifier again

-- Double is a type whose values are floating point numbers
doubleVal :: Double
doubleVal = 4.3
--           └─── Can have a fractional part unlike an Int


-- Char is a type whose values are characters
charVal :: Char
charVal = 'a'
--         └── The syntax for Char literals is single quotes around a character

-- Giving a definition a value that doesn't match it's signature
-- If you uncomment the definition exampleTypeError and try to load the module, you'll get a type error

--                      ┌─── We expect exampleTypeError to be an Int
-- exampleTypeError :: Int
-- exampleTypeError = 'a'
--                     └──── But 'a' is a Char

-- Which looks like:
--
-- src/Hello.hs:31:20: error:
--     • Couldn't match expected type ‘Int’ with actual type ‘Char’
--     • In the expression: 'a'
--       In an equation for ‘exampleTypeError’: exampleTypeError = 'a'
--
--    |
-- 31 | exampleTypeError = 'a'
--    |                    ^^^


---- Function basics

--          ┌──────────── The type we pass into the function
--          │  ┌──────── -> makes a function type
--          │  │   ┌──── The type we return from the function
plusOne :: Int -> Int -- Read this as "plusOne is a function that takes an Int and returns an Int"
plusOne n  = n + 1
--      │    ──┬──
--      │      └──────── Everything to the right of the = is called the right hand side of the definition
--      │                or "RHS" for short.
--      │                The RHS can refer to the function parameters that get bound in the LHS
--      └─────────────── Everything between the name and the = is called the left hand side
--                       or "LHS" for short.
--                       The LHS is where you give names to the parameters of the function

--           ┌────────────── A function can take multiple parameters by using more than one ->
--           │              All types before the final -> are parameters
--           │         ┌──── The last type is always the return type
--      ─────┴─────   ─┴─
plus :: Int -> Int -> Int -- Read this as "plus is a function that two Ints and returns an Int"
plus n m = n + m          -- "plus" adds the parameters together using the "+" operator
--   │ └──────────────────── We give the name "m" to the second parameter
--   └────────────────────── We give the name "n" to the second parameter
--

-- multiply
times :: Int -> Int -> Int
times n m = n * m

-- raise to a power
pow :: Int -> Int -> Int
pow n m = n ^ m

---- Calling functions

-- The syntax to call a function is just: name param1 param2 param3 ...
double :: Int -> Int
--          ┌─────────── the function we're calling
--          │    ┌────── 2 is the first parameter
--          │    │ ┌──── x is the second parameter
double x = times 2 x


-- You can pass the result of calling one function to another function by wrapping the call in parens
doubleXPlusTripleY :: Int -> Int -> Int
--                                ┌──────────────────── The first parameter for plus is (double x)
--                                │           ┌──────── The second parameter for plus is (times y 3)
--                            ────┴───── ─────┴─────
doubleXPlusTripleY x y = plus (double x) (times 3 y)

-- Exercise 1: write a function that computes x*y + x*z + y*z
ex1 :: Int -> Int -> Int -> Int
ex1 x y z = x*(y+z) + y*z

---- let and where: making nested definitions

-- Here's a function with a bunch of nested function calls
sumOfPowers :: Int -> Int

--            calls plus
--            on pow x 3 and 
--            the result of (plus (pow x 2) x)
--                      |
--              ________|___________________________
--             |        |       calls plus        |
--             |        |     on pow x 2 and x    |
--             |        |      _______|__________ |              
--             |        |      |                | | 
sumOfPowers x = plus (pow x 3) (plus (pow x 2) x)
--                    calls pow      calls pow 
--                    with x and     with x and  
--                    3 as args      2 as args
--                        |              |
-- _______________________|______________|_

-- We can give the subexpressions names using let ... in
sumOfPowersWithLet :: Int -> Int
sumOfPowersWithLet x =
  let square = pow x 2
      cube   = pow x 3
      sum1   = plus square x
      sum2   = plus cube sum1
  in  sum2

-- Or using where
sumOfPowersWithWhere :: Int -> Int
sumOfPowersWithWhere x = sum2 where
  square = pow x 2
  cube   = pow x 3
  sum1   = plus square x
  sum2   = plus cube sum1

-- Or we can mix and match
-- This example is admittedly silly :p
sumOfPowersWithBoth :: Int -> Int
--                            let ... in and be used anywhere you can use an expression
--                                ────────────────────┴──────────────────
sumOfPowersWithBoth x     =    plus cube       (let square = pow x 2 in plus square x)
--      |           |           |    |          ___________  ________ _______________   
--name function   1 arg         |   defined          |             |        |
--                              |above as x**3     about to        |     def of "square" applies under
--                              |                 define s/t we're |     these circumstances. 
--                           outside f()          calling 'square' |     applying "plus" to "square" and "x"
--                         adds the result of cube                 |
--                         and the stuff in parens               def of "square"


  where -- where doesn't have to be on the same line as the definition
    cube = pow x 3

-- Exercise 2: Rewrite doupleXPlusTripleY using let and where
doupleXPlusTripleYWithLet :: Int -> Int -> Int
doupleXPlusTripleYWithLet x y =  let  doubleX = x * 2 
                                      tripleY = y * 3 
                                  in plus doubleX tripleY
--         

doubleXPlusTripleYWithWhere :: Int -> Int -> Int
doubleXPlusTripleYWithWhere x y = plus doubleX tripleY
  where 
    doubleX = x * 2
    tripleY = y * 3

---- Functions with type variables

-- Identifiers that appear in a type that start with a lower case letter are type variables
-- A type variable means the function works on any type

-- The identity function takes any value and returns it unchanged
id :: a -> a -- When a type variable appears more than once in a signature, it refers to the same type
             -- Since "a" is both the parameter type and the return type, we have to return a value
             -- of the same type that we get in
id x = x

-- const is a function that takes two values, returns the first and ignores the second
--            ┌──────── The two parameters can be different types
--       ┌────┤    ┌─── The return type is the same type as the first parameter
const :: a -> b -> a
const x _ = x
--      └───────────── _ in the LHS will match anything and not bind a name


---- Tuples

-- A tuple is a type that holds multiple values

somePair :: (Int, Char) -- The syntax for a tuple type  is (Type1, Type2, Type2, ...)
somePair = (0, '0')     -- The syntax for a tuple value is (value1, value2, value3, ...)

-- Make a pair of the same value
twoOfThem :: a -> (a, a)
twoOfThem x = (x, x)

--       ┌─────────── fst takes a single parameter, which is a pair of an "a" and a "b"
--     ──┴───
fst :: (a, b) -> a
fst (a, _) = a
--  ──┬──
--    └────────────── You can pattern match on a tuple to gives names to it's sub components

-- Exercise 3: fill in these definitions
snd :: (a, b) -> b
snd (a, b) = b 
-- different from snd a b = b, which would mean taking two separate args and returning the 2nd.
-- instead, (a,b) is a tuple; it's one arg. snd returns the second part.

threeOfThem :: a -> (a, a, a)
threeOfThem a = (a, a, a)

fstOf3 :: (a, b, c) -> a
fstOf3 (a, b, c) = a

sndOf3 :: (a, b, c) -> b
sndOf3 (a, b, c) = b

thirdOf3 :: (a, b, c) -> c
thirdOf3 (a, b, c) = c

-- Exercise 4:
-- a) What is the type of ex3Mystery?
--a tuple? (a, b) -> (b,a)
-- b) Describe what it does.
-- it takes a tuple and returns that tuple with the value order swapped
-- c) Give it a better name
--tupleswap?
-- d) Define a second version of it using pattern matching instead of fst and snd
-- tupleswap (a,b) = (b, a)

ex3Mystery x = (snd x, fst x)
ex3Mystery :: (b1, b2) -> (b2, b1)

-- Exercise 5: fill in the definition
firsts :: (a, b) -> (c, d) -> (a, c)
firsts (a,b) (c,d) = (a,c)

seconds :: (a, b) -> (c, d) -> (b, d)
seconds (a, b) (c, d) = (b, d)

inners :: (a, b) -> (c, d) -> (b, c)
inners (a, b) (c, d) = (b, c)

outers :: (a, b) -> (c, d) -> (a, d)
outers (a, b) (c, d) = (a, d)

-- The unit type () has only one value ()
-- It's like a 0-tuple
unit :: ()
unit = ()

-- Exercise 6: fill in the definition
noneOfThem :: a -> ()
noneOfThem a = ()
--WHY NOT THIS? noneOfThem a = None or noneOfThem a = nil
 

---- Type class basics

--               ┌───────────────── This contraints "a" to types that are in Num
--               │                  Num is the class of types that are like numbers
--               │                  It lets you use +, -, *, and some other operations
--               │   ┌───────────── => Seperates the context from the type
--               │   │       ┌───── Right of the => is the normal type signature
--             ──┴── │  ─────┴─────
genericPlus :: Num a => a -> a -> a
genericPlus a b = a Prelude.+ b

-- Int is in Num so we can use genericPlus with Ints
fiveInt :: Int
fiveInt = genericPlus 2 3

-- Double is in Num so we can use genericPlus with Doubles
fiveDouble :: Double
fiveDouble = genericPlus 1.5 3.5

-- Char is not in Num, so this would be a type error
-- fiveChar = '2' + '3'


----- Operators

-- Functions that have two parameters and have names that use symbols instead
-- of alphanumeric characters are called operators and can be used infix

infixl 6 + -- + associates to the left with precedence 6
(+) :: Num a => a -> a -> a
a + b = a Prelude.+ b
--        ────┬────
--            └───────── We are defining our (+) using the normal (+) from the Prelude

infixl 7 * -- * associates to the left with precedence 7
(*) :: Num a => a -> a -> a
a * b = a Prelude.* b

infixl 8 ^ -- ^ associates to the left with precedence 8
--                ┌───── Integeral is the class of number types without fractional parts
(^) :: (Num a, Integral b) => a -> b -> a
a ^ b = a Prelude.^ b

--- Operators precedence and association

-- Arithmetic operators in Haskell use the order of operations you expect
operEx1 = 2 + 3 * 4 ^ 2 + 5
-- is the same as
operEx2 = (2 + (3 * (4 ^ 2))) + 5
--                  ───┬───
--                     └──────────── ^ has the highest precedence, 8, so it binds the tightest
--             ───┬────────
--                └───────────────── * has the next highest precedence, 7
--       ───┬────────────────┬────
--          ├────────────────┴────── + has the lowest precedence, 6
--          └─────────────────────── + the left + binds tighter than the right +
--                                   because we declared + to be left associative

-- Lets define upside-down land operators with different precedence
-- that associate to the right instead of the left
infixr 8 !+
(!+) :: Int -> Int -> Int
a !+ b = a + b

infixr 7 !*
(!*) :: Int -> Int -> Int
a !* b = a * b

infixr 6 !^
(!^) :: Int -> Int -> Int
a !^ b = a ^ b

-- Exercise 7: insert parens into the definitions with(out?) changing their values
ex7a = (3  * 4)  + (2  * (6  ^ 2))
ex7b = (3 !* ((4 !+ 2) !* 6))!^ 2
-- in order of precedence, highest to lowest: !+, !*, !^ 
ex7c = (2  ^ 3)  ^ 2
ex7d = 2 !^ (3 !^ 2)
-- read right to left.
-- You can use operators prefix and normal functions infix. These are all equivalent
plus_v0 a b = a + b
plus_v1 a b = (+) a b    -- wrap an operator in ( ) to make it prefix
plus_v2 a b = plus a b
plus_v3 a b = a `plus` b -- wrap a normal function in ` ` to make it infix

-- Exercise 8:
ex8a = 2 + 3 * 4 -- rewrite using + and * prefix
ex8ai = (+) 2 ((*) 3 4)

ex8b = 2 + 3 * 4 -- rewrite using plus and times infix
ex8bi = 2 `plus` (3 `times` 4)
--         BACKTICKS! NOT SINGLE QUOTES!

---- Booleans and comparisions
-- Lets define our first type!

--      ┌──────────── Type name must start with a capital letter
-- data Bool where
--   ┌────────────── Constructor name must start with a capital letter
--   True  :: Bool -- True  is value of type Bool
--   False :: Bool -- False is a value of type Bool
--   └────────────── Constructor name must start with a capital letter
--
-- NOTE: We're importing the one from the Prelude so we can use some
--       builtin syntax later, but the definition is the same.

-- Functions on types with more than once constructor can be defined
-- by cases on the different constructor falues

-- Logical Negation
not :: Bool -> Bool
not True  = False
not False = True

-- Logical and
infixr 3 &&
(&&) :: Bool -> Bool -> Bool
True && True = True
_    && _    = False

-- Exercise 9: fill in the definition
-- Logical or
infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) _ _ = True


(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True


-- This also works but it's clunky.
(||) :: Bool -> Bool -> Bool
(||) False False = False
(||) True False = True
(||) False True = True
(||) True True = True


-- Now lets make some comparision operators

-- infix instead of infixl or infixr means that they don't associate
infix 4 ==, /=, <, <=, >, >=
--comparisons are typically binary and independent 
--so can't chain so association doesn't make sense
--i.e., a < b < c doesn't have a clear meaning 


-- Eq is the class of types that can be compared for equality
-- Haskell used /= for "not equality" instead of != like a lot of other languages
(==), (/=) :: Eq a => a -> a -> Bool
a == b = a Prelude.== b
a /= b = a Prelude./= b

-- Ord is the class of types that have a total ordering
(<), (<=), (>), (>=) :: Ord a => a -> a -> Bool
a <  b = a Prelude.<  b
a <= b = a Prelude.<= b
a >  b = a Prelude.>  b
a >= b = a Prelude.>= b

-- Exercise 10: write the type and define the function

-- tests if a number is positive
positive :: Num a, Ord a => a -> Bool
positive a = a > 0
-- Aren't all nonimaginary numbers automatically Ord?
-- Lots of things are Ord but not numbers; it's anything with an ordering
-- A string has an ordering (aa < ab)
-- Lists of numbers have orders

-- inRange a b c is true if c is between a and b (inclusive)
inRange :: Ord a => a -> a -> a -> Bool
--This looks wrong but I don't know why.
inRange a b c = a <= c && c <= b

-- outOfRange a b c is true if c not is between a and b (exclusive)
outOFRange :: Ord a => a -> a -> a -> Bool
outOfRange a b c = c < a || c > b

outOFRange' :: Ord a => a -> a -> a -> Bool
outOfRange' a b c = not (inRange a b c)


--type signatures are much harder for me than function definitions!

--- Integral operations

-- integer divition
div :: Integral a => a -> a -> a
div a b = a `Prelude.div` b

-- integer modulus
mod :: Integral a => a -> a -> a
mod a b = a `Prelude.mod` b

-- Exercise 11: write the type and define the function

-- tests if an integral number is even
even :: Integral a => a -> Bool
even a = a `mod` 2 == 0

even' :: Integral a => a -> Bool
even' a = mod a 2 == 0


-- tests if an integral number is odd
odd :: Integral a => a -> Bool
odd a = mod a 2 == 1 
odd' :: Integral a => a -> Bool
odd' a = a `mod` 2 == 1 


-- given a b, returs a pair (c, d) such that a == b * c + d
divWithRemainder :: (Integral a) => a -> a -> (a, a)
divWithRemainder a b = floor(a/b), a `mod` b
--don't use floor -- the div operator

divWithRemainder' :: (Integral a) => a -> a -> (a, a)
divWithRemainder' a b = (a `div` b, a `mod` b)

---- Conditionals

-- The famous Collatz function
collatzStep :: Int -> Int
--                    ┌───────────────────────────────── The condition must be a Bool
--                    │            ┌─────────────┬────── The then and else branches must have the same type
--                 ───┴──      ────┴────      ───┴───
collatzStep n = if even n then n `div` 2 else 3*n + 1
--    n is arg      |              |             |
--              if it's even    divide by 2     else 3n+1

-- 3 parts: if, then, else
-- ---- if evaluates to a Bool
   -- --then and else must return the same type


-- in Haskell we can write our own if function
if' :: Bool -> a -> a -> a
--     args are Bool    --returns one value of the same type
--     and two values
if' True  thenVal _       = thenVal
--  if the condition is
  -- True, return val1
if' False _       elseVal = elseVal
--  if the condition is
  -- False, return val2




-- or equivilently
if'' :: Bool -> a -> a -> a
-- takes a Bool and 2 values, returns 1 value

if'' cond thenVal elseVal = if cond then thenVal else elseVal
--     |
--  a param; the condition being tested


collatzStep' :: Int -> Int
collatzStep' n = if' (even n) (n `div` 2) (3*n + 1)

-- You can also test conditions using guards

-- Haskellers like this alias to make reading guards more natural
-- otherwise :: Bool
-- otherwise = True

collatzStepWithGuards :: Int -> Int
collatzStepWithGuards n | even n    = n `div` 2
                        | otherwise = 3*n + 1

--Guards are just a way to specify diff behaviors for diff conditions?
--Effectively mean "if"?

-- Guards are handy if you have more than two conditions
-- and are tested from top to bottom


data CharClass where
  LowerCase  :: CharClass
  UpperCase  :: CharClass
  Digit      :: CharClass
  WhiteSpace :: CharClass
  Unknown    :: CharClass
  deriving  -- This tells the compiler that we want CharClass to be in the following classes
            -- with the standard definitons for their operations
    ( Eq    -- Two values are equal if they have the same consturctor
    , Ord   -- The generated ordering is the same as the order we listed the constructors in
    , Show  -- This will let the repl print values of type CharClass
    )

classifyChar :: Char -> CharClass
classifyChar c
  | inRange 'a' 'z' c      = LowerCase
  | inRange 'A' 'Z' c      = UpperCase
  | inRange '0' '9' c      = Digit
  | c == ' '  || c == '\t' = WhiteSpace
  | c == '\n' || c == '\r' = WhiteSpace
  | otherwise              = Unknown

-- Exercise 12: rewrite classifyChar using if expresions instead of guards
classifyCharWithIf :: Char -> CharClass
classifyCharWithIf c = 
  if (inRange 'a' 'z' c) 
    then LowerCase 
    else if (inRange 'A' 'Z' c) 
      then UpperCase 
      else if inRange '0' '9' c 
        then Digit 
        else if (== ' '  || c == '\t') 
          then WhiteSpace 
          else if (== '\n' || c == '\r') 
            then WhiteSpace 
            else Unknown

-- Exercise 13: write isLower reusing the classifyChar function
isLower :: Char -> Bool
isLower c = if classifyChar c == LowerCase 
  then True
  else False




-- Exercise 13: Write a classifyInt function with at least 4 cases
-- Feel free to classify them however you want!
data IntClass =  Pos_One_Digit | Pos_Two_Digit | Pos_Three_Digit | Dunno
classifyInt :: Int -> IntClass
classifyInt a
  | 1 >= a && a <=9     = Pos_One_Digit
  | 10 >= a && a <=99      = Pos_Two_Digit
  | 100 >= a && a <=999    = Pos_Three_Digit
  | otherwise          = Dunno




-- Exercise 14: (hard) define the function
-- Given an int n, return the number of times you have to apply collatzStep to get to 1
-- ex: collatzCount 1 == 0
--     collarzCount 2 == 1 b/c collatzStep 2 == 1
--     collatzCount 5 == 5 b/c collatzStep  5 == 16
--                             collatzStep 16 ==  8
--                             collatzStep  8 ==  4
--                             collatzStep  4 ==  2
--                             collatzStep  2 ==  1
collatzCount :: Int -> Int
collatzCount n 
  | n == 1                 = 0
  | otherwise              = 1+collatzCount (collatzStep n)

---- List literals and ranges
oneToFive :: [Int]      -- [Type] means a list of Type
oneToFive = [1,2,3,4,5] -- A list litereral is values in [] seperate ,'s

oneToFiveRange :: [Int]
-- [a..b] generates a list with the values from a to b including the ends
oneToFiveRange = [1..5]

tenEightSixFourTwo :: [Int]
-- [a,b..c] generates a list with the vales from a to c
-- where the different between successive values is (b - a)
tenEightSixFourTwo = [10,8 .. 1]

lowercaseChars :: [Char]
-- Ranges can be used with any type in Enum, not just numbers
lowercaseChars = ['a'..'z']

---- Type synonyms and String literals

-- The type keyword gives a new name to an existing type that can be used interchangeably
-- A String is just a list of Char, which means you can use a String like any any other list
type String = [Char]

-- Strings are only special in that they have syntactic sugar
stringVal :: String
stringVal = "some string"

-- Exercise 15: rewrite stringVal using list literal syntax
stringValWithListSyntax :: String
stringValWithListSyntax = ['s','t','r','i','n','g']
-- == "string"



-- You can use string syntax in patterns too
knockKnock :: String -> String
knockKnock "Banana" = "Banana who?"
knockKnock other    = other ++ " you glad I didn't say, \"Banana\"?"

---- lambdas, case, and partial application, and operator sections

-- Functions can be used like any other value. Put in lists, passed as parameters to
-- other functions etc.

arithmeticOperations :: [Int -> Int -> Int]
arithmeticOperations = [plus, times]

-- Lambda syntax lets you create a function without giving it a name.
stepBy :: Int -> Int -> Int -> [Int]
stepBy step start end = [start, start+step .. end]
-- can be written with lambda syntax as
stepByWithLambda :: Int -> Int -> Int -> [Int]
stepByWithLambda = \step start end -> [start, start+step .. end]

-- A pair of projections from 4-tuples
someProjections :: ((a1, b1, c1, d1) -> b1, (a2, b2, c2, d2) -> d2)
-- LHS of the lambda on the left    RHS of th lambda on the left
--                   ┴──┴──┴──┴     ┴
someProjections = (\(_, b, _, _) -> b, \(_, _, _, d) -> d)
--                                       ┬──┬──┬─┬─     ┬
--                   LHS of the lambda on the right     RHS of the lambda on the right

-- lambdas can only have one clause, but you can pattern patch in an expression with case ... of
knockKnockWithLambdaAndCase :: String -> String
knockKnockWithLambdaAndCase = \whosThere -> case whosThere of
  "Banana" -> "Banana who?"
  other    -> other ++ " you glad I didn't say, \"Banana\"?"

--- Partial Application
-- (->) is right associative so these types are the same
type FunOf3IntsToIntV1 = Int -> Int -> Int -> Int

type FunOf3IntsToIntV2 = Int -> (Int -> (Int -> Int))

type FunOf3IntsToIntV3 = Int -> FunOf2IntsToInt
type FunOf2IntsToInt   = Int -> FunOfIntToInt
type FunOfIntToInt     = Int -> Int

-- And these definitions are all the same
plusPartialV0, plusPartialV1, plusPartialV2 :: Int -> Int -> Int
plusPartialV0 a b = plus a b
plusPartialV1 a   = plus a
plusPartialV2     = plus

-- Or with lambda syntax
plusPartialV3, plusPartialV4 :: Int -> Int -> Int
plusPartialV3 = \a b -> plus a b
plusPartialV4 = \a   -> plus a

-- You can use operations prefix by wrapping them in parens
powPartialV0, powPartialV1, powPartialV2 :: (Num a, Integral b) => a -> b -> a
powPartialV0 a b = (^) a b
powPartialV1 a   = (^) a
powPartialV2     = (^)

-- You can partially apply a value on either side of the operator
twoToThe :: (Num a, Integral b) => b -> a
twoToThe = (2^)

squared :: Num a => a -> a
squared = (^2)

---- Higher order functions

-- lets play around with some functions that take function arguments

-- map applies a function to every element of a list
map :: (a -> b) -> [a] -> [b]
map = Prelude.map

tens :: [Int]
tens = map (* 10) [1..9]

powersOfTwo :: [Int]
powersOfTwo = map twoToThe [1..63]

-- filter returns the values of a list for which a predicate is true
filter :: Ord a => (a -> Bool) -> [a] -> [a]
filter = Prelude.filter

caps :: String -> String
caps = filter (\c -> not (isLower c))

-- Exercise 16:
-- A couple functions you'll need
-- get the length of a list
length :: [a] -> Int
length = Prelude.length

-- concatenate two lists. This works with Strings, so: "a" ++ " " ++ "b" == "a b"
infixr 5 ++
(++) :: [a] -> [a] -> [a]
(++) = (Prelude.++)

-- if the arg is 0..9, 0 -> zero, 1 -> "one", etc
-- n < 0 or n >= 10 to "?"
singlesDigitToWord :: Int -> String
singlesDigitToWord = undefined

-- 11 -> "eleveven", 32 -> "thirty two", etc
-- n < 0 or n >= 100 to "?"
twoDigitNumberToWord :: Int -> String
twoDigitNumberToWord = undefined

-- find all numbers between 0 and 100 which are shorter than their words
numbersShorterThanTheirWords :: [Int]
numbersShorterThanTheirWords = undefined

---- ap and compose

-- ($) lets you apply a to a value, but it has low precedence
-- so it can be used as an alternative to ( ) for grouping
infixr 0 $
($) :: (a -> b) -> a -> b
f $ x = f x

-- (.) is function composition. It lets you chain functions together
-- It has high precedence so it binds tighter than most other operators
infixr 9 .
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- sorts a list
sort :: Ord a => [a] -> [a]
sort = List.sort

multiplesOfThreeInAlphabeticalOrder :: [String]
multiplesOfThreeInAlphabeticalOrder
 = sort
 . map twoDigitNumberToWord
 . filter (\n -> n `mod` 3 == 0)
 $ [0..99]
