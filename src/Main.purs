module Main where

import Prelude (Unit, unit, ($), class Ord, show, class Semiring, add, zero, mul
                , (++), (-), (>=), otherwise)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.List (List(Nil, Cons), take, drop)
import Data.Map (Map, lookup, keys)
import Data.Maybe (Maybe(Just, Nothing))
import Unsafe.Coerce (unsafeCoerce)
import Data.Foldable (class Foldable, foldl)

todo :: forall a. a
todo = unsafeCoerce unit

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Run tests: 'pulp test'"

type Message k = List k
type Dictionary k v = Map k v

toNumber :: forall f k. (Foldable f, Semiring k) => k -> f k -> k
toNumber b xs = foldl (\x y -> add (mul x b) y) zero xs

-- TODO: abstract over String and Int
decipher :: Dictionary Int String -> Message Int -> String
decipher d m = snd t
  where
    max :: forall a. (Ord a) => a -> a -> a
    max x y | x >= y    = x
            | otherwise = y

    maxDg = foldl max zero $ keys d
    t = loop d toKey m maxDg ""
    toKey = (toNumber 10)

-- TODO: abstract over String and Int
findCode :: Dictionary Int String -> (Message Int -> Int) -> Message Int -> Tuple Int String
findCode d toKey cs =
  case lookup (toKey cs) d of
    Just x -> Tuple l x
    Nothing -> case cs of
                Cons x Nil -> Tuple 1 $ show x
                _ -> findCode d toKey $ take (l - 1) cs
  where
    l = Data.List.length cs

-- TODO: abstract over String and Int
-- TODO: abstract return type is a monoid
loop :: Dictionary Int String -> (Message Int -> Int) -> Message Int -> Int -> String -> Tuple Int String
loop d toKey m tn acc =
  case mpen of
    Nil -> Tuple np (acc ++ code)
    _ -> loop d toKey mpen tn (acc ++ code)
  where
    es = take tn m
    t = findCode d toKey es
    np = fst t
    code = snd t
    mpen = drop np m
