{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Solutions.FMCNat where

-- Do not alter this import!

import Prelude
  ( Bool (..),
    Eq (..),
    Integral (..),
    Num (..),
    Ord (..),
    Show (..),
    String,
    error,
    not,
    otherwise,
    undefined,
    ($),
    (&&),
    (++),
    (.),
    (||),
  )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where
  -- zero  should be shown as O
  -- three should be shown as SSSO
  show O = "O"
  show (S n) = "S" ++ show n

instance Eq Nat where
  (==) n m = case n - m of
    O -> True
    _ -> False

instance Ord Nat where
  (<=) :: Nat -> Nat -> Bool
  (<=) O _ = True
  (<=) (S _) O = False
  (<=) (S n) (S m) = n <= m

  -- Ord does not REQUIRE defining min and max.
  -- Howevener, you should define them WITHOUT using (<=).
  -- Both are binary functions: max m n = ..., etc.
  min n m = case isZero (n <-> m) of
    True -> n
    False -> m

  max n m = case isZero (n <-> m) of
    True -> m
    False -> n

----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

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

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred n = n <-> S O

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd n = case even n of
  True -> False
  False -> True

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O = n
n <+> S m = S (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
monus :: Nat -> Nat -> Nat
monus = (<->)

(<->) :: Nat -> Nat -> Nat
n <-> O = n
O <-> _ = O
S n <-> S m = n - m

infixl 6 <+>, <->

-- multiplication
times :: Nat -> Nat -> Nat
times _ O = O
times n (S m) = n + times n m

(<*>) :: Nat -> Nat -> Nat
(<*>) = times

infixl 7 <*>

-- power / exponentiation
pow :: Nat -> Nat -> Nat
pow _ O = one
pow n (S m) = n * pow n m

exp :: Nat -> Nat -> Nat
exp = pow

(<^>) :: Nat -> Nat -> Nat
(<^>) = pow

infixr 8 <^>

-- quotient
(</>) :: Nat -> Nat -> Nat
(</>) n O = error "Não é possível dividir por O"
(</>) O n = O
(</>) n m = case n <-> m of
  O -> case m <-> n of
    O -> S O
    _ -> O
  n -> S ((</>) n m)

-- remainder
(<%>) :: Nat -> Nat -> Nat
(<%>) _ O = undefined
(<%>) n m = n <-> (m * (n </> m))

-- euclidean division
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv = undefined

-- divides
(<|>) :: Nat -> Nat -> Bool
(<|>) O _ = True
(<|>) _ O = False
(<|>) n m = case m <%> n of
  O -> True
  _ -> False

divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
dist :: Nat -> Nat -> Nat
dist n m = case n <= m of
  True -> m - n
  False -> n - m

(|-|) = dist

factorial :: Nat -> Nat
factorial O = S O
factorial n = n * factorial (n - one)

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = zero
sg n = one

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a = case a < b of
  True -> O
  False -> S (lo b (a </> b))

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: (Integral a) => a -> Nat
toNat x = case x < 0 of
  True -> error "toNat não suporta números negativos"
  False -> case x == 0 of
    True -> O
    False -> S (toNat (x - 1))

fromNat :: (Integral a) => Nat -> a
fromNat O = 0
fromNat (S n) = 1 + fromNat n

-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where
  (+) = (<+>)
  (*) = (<*>)
  (-) = (<->)
  abs n = n
  signum :: Nat -> Nat
  signum = sg
  fromInteger x
    | x < 0 = error "Negativos não são suportados"
    | x == 0 = O
    | otherwise = S (fromInteger (x - 1))
