module Main where

import Prelude (Unit, unit, ($), class Ord, class Show, show, class Semiring, add, zero, mul
                , (++), (-), bind, (>=), otherwise)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.List (List(Nil, Cons), toList, take, drop)
import Data.Map (Map, fromList, lookup, keys)
import Data.Maybe (Maybe(Just, Nothing))
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (class Foldable, foldl)

todo :: forall a. a
todo = unsafeCoerce unit

message :: Message Int
message = toList [1, 2, 3, 2, 3, 4, 3, 2, 3, 2, 3, 4, 4, 8]

dictionary :: Dictionary Int String
dictionary = fromList $ toList [ (Tuple 23 "N"), Tuple 234 "T", Tuple 8 "Q" ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Message= " ++ show message
  log $ "Dictionary= " ++ show dictionary
  newline
  log $ "decipher= " ++ decipher dictionary message
  where
  newline = log " "

type Message k = List k
type Dictionary k v = Map k v

-- TODO: abstract over String and Int
--decipher :: forall k. (Ord k, Show k, Semiring k) => Dictionary k String -> Message k -> String
decipher :: Dictionary Int String -> Message Int -> String
decipher d m = snd t
  where
    max :: forall a. (Ord a) => a -> a -> a
    max x y | x >= y    = x
            | otherwise = y
    maxDg = foldl max 0 $ keys d
    t = fx d m maxDg ""

toNumber :: forall f a. (Foldable f, Semiring a) => a -> f a -> a
toNumber b xs = foldl (\x y -> add (mul x b) y) zero xs

-- TODO: abstract over String and Int
-- TODO: search in dictionary
-- Search cs list options in dict
-- returns the finded code as a pair (length key, value)
findCode :: Dictionary Int String -> Message Int -> Tuple Int String
findCode d Nil = Tuple 1 "ERROR"
findCode d cs =
  case lookup (toNumber 10 cs) d of
    Just x -> Tuple l x
    Nothing -> case cs of
                Cons x Nil -> Tuple 1 $ show x
                _ -> findCode d $ take (l- 1) cs
  where
    l = Data.List.length cs
-- d dict
-- m current msg
-- take tn
-- accumulator String
-- TODO: abstract over String and Int
fx :: Dictionary Int String -> Message Int -> Int -> String -> Tuple Int String
fx d m tn acc =
  case mpen of
    Nil -> Tuple np (acc ++ code)
    _ -> fx d mpen tn (acc ++ code)
  where
    es = take tn m
    t = findCode d es
    np = fst t
    code = snd t
    mpen = drop np m
