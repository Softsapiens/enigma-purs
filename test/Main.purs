module Test.Main where

import Prelude (Unit, unit, ($), (+), class Ord, bind, show, (++))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Data.List (List(Nil, Cons), toList)
import Data.Map (Map, fromList, lookup)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Foldable (class Foldable, foldl)

import Main (Message, Dictionary, decipher, findCode, fx, message, dictionary)

msgTest :: Message Int
msgTest = toList [1, 2, 3, 2, 3, 4]

dictTest :: Dictionary Int String
dictTest = fromList $ toList [ (Tuple 23 "N"), Tuple 234 "T" ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let d = dictionary
  log $ "Testing decipher with dictionary: " ++ show d
  log $ "Message= " ++ show message
  log $ "Dictionary= " ++ show dictionary
  newline
  log $ "findCode= " ++ snd (findCode dictionary (toList [2, 3, 4]))
  log $ "fx= " ++ show (fx dictionary (toList [1, 2, 3, 2, 3, 4, 3, 2, 3, 2, 3, 4]) 3 "")
  newline
  log $ "decipher= " ++ decipher dictionary message
  newline
  log $ decipher d $ toList [1]
  log $ decipher d $ toList [2, 3]
  where
  newline = log " "
