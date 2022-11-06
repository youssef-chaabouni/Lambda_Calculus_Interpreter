module Cmd where

import Expr

data Cmd = Eval LExp | Let Var LExp | Noop | Quit | Load String | Error String
  deriving (Show,Read)
