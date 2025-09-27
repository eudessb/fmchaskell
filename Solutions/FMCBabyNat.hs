module FMCBabyNat where

-- Do not alter this import!
import Prelude (Eq (..), Show (..), undefined)

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

-- define a new data type called Nat by listing all forms
data Nat = O | S Nat
  deriving (Eq, Show)

-- some sugar
zero, one, two, three, four, five, six, seven, eight :: Nat
zero = O
one = S zero
two = S one
three = S two
four = S three
five = S four
six = S five
seven = S six
eight = S seven

-- addition
(+) :: Nat -> Nat -> Nat
n + O = n
n + S m = S (n + m)

-- syntactic associativity: L
-- syntactic precedence: 6
infixl 6 +

-- Output: O means False, S O means True
isZero :: Nat -> Nat
isZero O = O
isZero n = n

-- pred is the predecessor but we define zero's to be zero
-- Feito
pred :: Nat -> Nat
pred O = O
pred n = n -* S O

-- Output: O means False, S O means True
even :: Nat -> Nat
even O = S O
even (S O) = O
even (S (S n)) = even n

odd :: Nat -> Nat
odd O = O
odd n = case even n of
  O -> S O
  S O -> O

-- This is called the dotminus or monus ope>rator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (-*)

-- Feito ?
(-*) :: Nat -> Nat -> Nat
n -* O = n
O -* _ = O
S n -* S m = n -* m

-- multiplication
-- Nova abordagem sem o uso de -*
(*) :: Nat -> Nat -> Nat
n * O = O
n * S m = n + (n * m)

infixl 7 *

-- exponentiation
(^) :: Nat -> Nat -> Nat
m ^ O = S O
n ^ S m = n ^ m * n

infix 8 ^

-- quotient
(/) :: Nat -> Nat -> Nat
_ / O = undefined
O / _ = O
n / m = case n -* m of
  O -> case m -* n of
    O -> S O
    _ -> O
  n -> S (n / m)

-- remainder
(%) :: Nat -> Nat -> Nat
(%) = undefined

-- divides
-- just for a change, we start by defining the "symbolic" operator
-- and then define `devides` as a synonym to it
-- again, outputs: O means False, S O means True
(|||) :: Nat -> Nat -> Nat
(|||) = undefined

-- x `absDiff` y = |x - y|
-- (Careful here: this - is the actual minus operator we know from the integers!)
absDiff :: Nat -> Nat -> Nat
absDiff = undefined

(|-|) :: Nat -> Nat -> Nat
(|-|) = absDiff

-- Com -*
factorial :: Nat -> Nat
factorial O = S O
factorial n = n * factorial (n -* one)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg = undefined

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo = undefined
