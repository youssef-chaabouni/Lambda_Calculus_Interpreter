import System.IO
import Control.Monad.Trans
import System.Console.Haskeline

import Expr
import Cmd
import Subst
import Eval
import Parser

import Data.List
import PrettyExpr (printLExp)

-- an "environment" is a list of variable names paired with their definitions as lambda-expressions
type Env = [(Var,LExp)]

-- undefinedVar determines whether an expression has any free variable that is not defined by the environment
undefinedVar :: Env -> LExp -> Maybe Var
undefinedVar env t = find (\y -> lookup y env == Nothing) (free t)

type Repl a = InputT IO a

goFromFile :: Env -> [String] -> Repl ()
goFromFile env [] = go env
goFromFile env (inst:insts) = do
  case runParser parseCmd inst of             -- parse the input as a command
    Nothing -> do
      liftIO $ putStrLn "Parse Error!"
      return ()
    Just (cmd, _) -> do
      case cmd of                
        Eval t ->                             -- execute an eval command
          -- the expression to be evaluated cannot have any free
          -- variables that are not defined in the environment
          case undefinedVar env t of
            Just y -> (liftIO $ putStrLn ("Variable not in scope: " ++ y)) >> goFromFile env insts
            Nothing -> do
              -- substitute for all variables defined in the environment,
              -- in order from left to right
              let t' = foldl (\t (x,u) -> subst (u,x) t) t env
              -- normalize the resulting term
              let u = normalize t'
              -- print the result
              liftIO $ printLExp u 
              -- continue the REPL
              goFromFile env insts
              
        Let x t ->             -- execute a let command
          case undefinedVar env t of
            Just y -> (liftIO $ putStrLn ("Variable not in scope: " ++ y)) >> goFromFile env insts
            Nothing -> do
              -- continue the REPL in an environment extended with x=t
              goFromFile ((x,t):env) insts
            
        Noop -> goFromFile env insts         -- execute a no-op command, by doing nothing and continuing the REPL

        Quit -> do             -- execute a quit command, by terminating the REPL
          liftIO $ putStrLn "Leaving Lambda Calculus Interpreter."
          liftIO $ putStrLn "Goodbye :)"
          return ()

        --Load filename -> do             -- execute a load command, by executing instructions in 'filename'
        --  return ()

        Error str -> do             -- Error
          liftIO $ putStrLn ("Parsing error in: `" ++ str ++ "`")
          >> goFromFile env insts

go :: Env -> Repl ()
go env = do
  mline <- getInputLine "Lambda> "
  case mline of
    Nothing -> do
      liftIO $ putStrLn "Error while reading input!"
      return ()
    Just (line) -> do
      case runParser parseCmd line of             -- parse the input as a command
        Nothing -> do
          liftIO $ putStrLn "Parse Error!"
          return ()
        Just (cmd, _) -> do
          case cmd of                
            Eval t ->                             -- execute an eval command
              -- the expression to be evaluated cannot have any free
              -- variables that are not defined in the environment
              case undefinedVar env t of
                Just y -> (liftIO $ putStrLn ("Variable not in scope: " ++ y)) >> go env
                Nothing -> do
                  -- substitute for all variables defined in the environment,
                  -- in order from left to right
                  let t' = foldl (\t (x,u) -> subst (u,x) t) t env
                  -- normalize the resulting term
                  let u = normalize t'
                  -- print the result
                  liftIO $ printLExp u 
                  -- continue the REPL
                  go env
                  
            Let x t ->             -- execute a let command
              case undefinedVar env t of
                Just y -> (liftIO $ putStrLn ("Variable not in scope: " ++ y)) >> go env
                Nothing -> do
                  -- continue the REPL in an environment extended with x=t
                  go ((x,t):env)
                
            Noop -> go env         -- execute a no-op command, by doing nothing and continuing the REPL

            Quit -> do             -- execute a quit command, by terminating the REPL
              liftIO $ putStrLn "Leaving Lambda Calculus Interpreter."
              liftIO $ putStrLn "Goodbye :)"
              return ()

            Load filename -> do             -- execute a load command, by executing instructions in 'filename'
              file <- liftIO $ readFile (filename)
              goFromFile env (lines file)

            Error str -> do             -- Error
              liftIO $ putStrLn ("Parsing error in: `" ++ str ++ "`")
              >> go env

-- the top-level read-eval-print-loop
repl :: Repl ()
repl = go []                                          -- start the interpreter in an empty environment

main :: IO ()
main = runInputT defaultSettings repl


-- > Let "zero" (L"f" (L"x" (V"x")))
-- > Let "id" (L"x" (V"x"))
-- > Eval (A (V"id") (V"zero"))
-- L "f" (L "x" (V "x"))
-- > Noop
-- > Quit