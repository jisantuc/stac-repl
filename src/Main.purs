module Main where

import Effect (Effect)
import Node.ReadLine (prompt)
import Prelude (Unit, bind)
import Repl (replProgram)

main :: Effect Unit
main = do
  interface <- replProgram
  prompt interface
