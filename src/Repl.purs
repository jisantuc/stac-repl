module Repl where

import Node.ReadLine

import Data.Array (filter)
import Data.String.Utils (startsWith)
import Effect (Effect)
import Effect.Class.Console (log)
import Prelude (Unit, bind, discard, pure, ($), (*>))

completions :: Array String
completions = [ "collection", "item" ]

lineHandler :: Interface -> String -> Effect Unit
lineHandler interface s = log s *> prompt interface

replProgram :: Effect Interface
replProgram = do
  interface <- createConsoleInterface ( \s ->
        pure
          $ { completions: filter (startsWith s) completions
            , matched: s
            }
    )
  setPrompt "stac > " 4 interface
  setLineHandler interface (lineHandler interface)
  pure $ interface
