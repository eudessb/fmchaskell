module ExBool where

-- Do not alter this import!

import GHC.Base qualified
import Prelude
  ( Char,
    Enum (..),
    Eq (..),
    Int,
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    error,
    otherwise,
    undefined,
    ($),
    (++),
    (.),
  )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where
  show :: Bool -> GHC.Base.String
  show True = "True"
  show False = "False"

instance Enum Bool where
  toEnum 0 = False
  toEnum 1 = True
  toEnum _ = error "toEnum: valor inválido, deve ser 0 ou 1!!"

  fromEnum :: Bool -> Int
  fromEnum False = 0
  fromEnum True = 1

-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
False && _ = False
True && b = b

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
(||) True _ = True
(||) False _ = False

infixr 2 ||

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
(/|\) p q = not (p && q)

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
(\|/) p q = not (p || q)

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
(<=/=>) = undefined

infixr 2 <=/=>

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True thenValue elseValue = thenValue
ifThenElse False thenValue elseValue = elseValue

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
p ==> q = not p || q

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
p <== q = q ==> p

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
(<=>) p q = (p <== q) && (p ==> q)

infixr 1 <=>
