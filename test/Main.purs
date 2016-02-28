module Test.Main where

import Prelude (Unit, ($), bind, show, (++))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Tuple (Tuple(Tuple))
import Data.List (toList)
import Data.Map (fromList)
import Main (Message, Dictionary, decipher)


message :: Message Int
message = toList [1, 2, 3, 2, 3, 4, 3, 2, 3, 2, 3, 4, 4, 8]

dictionary :: Dictionary Int String
dictionary = fromList $ toList [ (Tuple 23 "N"), Tuple 234 "T", Tuple 8 "Q" ]

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  let d = dictionary
  log $ "Testing decipher with dictionary: " ++ show d
  log $ "Message= " ++ show message
  log $ "Dictionary= " ++ show dictionary
  newline
  log $ "decipher= " ++ decipher dictionary message
  newline
  where
  newline = log " "
