module Main where

import Prelude (Unit, unit, ($), (+), class Ord, class Show, show, class Semiring, add, zero
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
message = toList [1, 2, 3, 2, 3, 4, 3, 2, 3, 2, 3, 4]

dictionary :: Dictionary Int String
dictionary = fromList $ toList [ (Tuple 23 "N"), Tuple 234 "T" ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ "Message= " ++ show message
  log $ "Dictionary= " ++ show dictionary
  log $ "findCode= " ++ snd (findCode dictionary (toList [2, 3, 4]))
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

toNumber :: forall f a. (Foldable f, Semiring a) => f a -> a
toNumber xs = foldl add zero xs

-- TODO: abstract over String and Int
-- TODO: search in dictionary
-- Search cs list options in dict
-- returns the finded code as a pair (length key, value)
findCode :: Dictionary Int String -> Message Int -> Tuple Int String
findCode d cs =
  case cs of
    Cons x Nil -> Tuple 1 $ show x
    Cons 2 (Cons 3 (Cons 4 Nil)) -> Tuple 3 "T"
    Cons 2 (Cons 3 Nil) -> Tuple 2 "N"
    Nil -> Tuple 0 ""
    _ -> findCode d $ take (Data.List.length cs - 1) cs

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
