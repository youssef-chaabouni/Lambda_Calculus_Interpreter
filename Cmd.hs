module Cmd where

import Expr

data Cmd = Eval LExp | Let Var LExp | Noop | Quit | Error String
  deriving (Show,Read)
