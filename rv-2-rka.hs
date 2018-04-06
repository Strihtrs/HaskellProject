-- rv-2-rka
-- Filip Kalous
-- xkalou03

-- Import libraries necessary for run the program
import Options.Applicative
import Data.Semigroup ((<>))
import System.IO
import Data.List
import Control.Exception
import System.Exit

-- Import custom data types module
import Types


-- Function does one step of the regular expression to finite automata algorithm
step :: String -> Either String Tree
step input = case foldl parse [] input of
    [x] -> Right x
    _   -> Left "Regular expression is not valid!"
    where 
          parse (r:l:t) '.' = Operation '.' l r : t
          parse (r:l:t) '+' = Operation '+' l r : t
          parse (l:t)   '*' = Star '*' l : t
          parse t char
            | char `notElem` ['*', '+', '.'] = Symbol char : t
            | otherwise = []

-- Transforms Tree representation to finite automata representation
-- End condition describes how to create finite state automata for transition from one state to another using one character
tree2fsm :: Tree -> FSM
tree2fsm (Symbol a) = FSM [1, 2] 1 [2] [TTransition 1 2 (TTLabel a)]
tree2fsm (Star c t) = fsmStar ( tree2fsm t )
tree2fsm (Operation o l r)
    | o == '+' = fsmUnion  (tree2fsm l) (tree2fsm r)
    | o == '.' = fsmConcat (tree2fsm l) (tree2fsm r)

-- Increments number by 1
inc :: Int -> Int
inc a = 1 + a

-- Increments numbers in array by given number
incList :: [Int] -> Int -> [Int]
incList a i = map (+i) a

-- Increments every state in transaction in array of transactions by 1
incTransitions :: [TTransition] -> [TTransition]
incTransitions = map (\(TTransition a b c) -> TTransition (inc a) (inc b) c)

-- Function defines how to expand iteration
fsmStar :: FSM -> FSM
fsmStar (FSM sA iA eA tA) = FSM (1 : getNewStates) 1 [getNewEndState] getNewTransitions
    where
          getNewStates      = incList sA 1 ++ [getNewEndState]
          getNewEndState    = 2 + length sA
          getNewTransitions = [TTransition 1 (inc iA) Epsilon, TTransition 1 getNewEndState Epsilon]
                              ++ incTransitions tA
                              ++ map (\s -> TTransition s getNewEndState Epsilon) (incList eA 1)
                              ++ map (\e -> TTransition e (inc iA) Epsilon) (incList eA 1)

-- Function defines how to expand union
fsmUnion :: FSM -> FSM -> FSM
fsmUnion (FSM sA iA eA tA) (FSM sB iB eB tB) = FSM getNewStates 1 [getNewEndState] getNewTransitions
    where
          fsmAEnd           = 1 + length sA
          getNewStates      = 1:[] ++ incList sA 1 ++ incList sB fsmAEnd ++ [getNewEndState]
          getNewEndState    = 2 + (length sA + length sB)
          getNewTransitions = [TTransition 1 (inc iA) Epsilon, TTransition 1 (fsmAEnd + iB) Epsilon]
                                ++ incTransitions tA
                                ++ map (\(TTransition a b c) -> TTransition (fsmAEnd + a) (fsmAEnd + b) c) tB
                                ++ map (\s -> TTransition s getNewEndState Epsilon) (incList eA 1)
                                ++ map (\s -> TTransition s getNewEndState Epsilon) (incList eB fsmAEnd)

-- Function defines how to expand concatenation
fsmConcat :: FSM -> FSM -> FSM
fsmConcat (FSM sA iA eA tA) (FSM sB iB eB tB) = FSM getNewStates iA getNewEndStates getNewTransitions
    where
          middleState       = 1 + length sA
          getNewEndStates   = incList eB middleState
          getNewStates      = sA ++ [middleState] ++ incList sB middleState
          getNewTransitions = tA 
                                ++ map (\s -> TTransition s middleState Epsilon) eA
                                ++ [TTransition middleState (iB + middleState) Epsilon]
                                ++ map (\(TTransition a b c) -> TTransition (middleState + a) (middleState + b) c) tB

-- Function prints finite state automata to output or error if transformation failed
printFSM :: Either String Tree -> String
printFSM (Right t) = show ( tree2fsm t )
printFSM (Left error) = error

-- Function prints internal representation to output or error if transformation failed
printInternal :: Either String Tree -> String
printInternal (Right t) = printTree t
printInternal (Left error) = error

-- Transforms internal representation to string
printTree :: Tree -> String
printTree (Symbol a) = [a]
printTree (Star o t) = printTree t ++ [o]
printTree (Operation o l r) = printTree l ++ printTree r ++ [o]

-- Depends on the arguments of the program, function decides
-- if should program store regular expression to internal representation and then print it
-- or it should continue and print equivalent finite automata
program :: Arguments -> IO ()
program (Arguments r t i)
    | r = putStrLn $ printInternal $ step i
    | t = putStrLn $ printFSM $ step i
    | otherwise = printErrorAndQuit "Wrong arguments!"

-- Function defines command arguments
-- -r - Program takes input regular expression, stores it to internal representation and then prints it to standard output
-- -t - Program takes input regular expression, stores it to internal representation, 
--      transforms it to finite automata and then prints the automata to output
-- INPUT - string which specifies path to input file. Default value is empty string
defineArguments :: Parser Arguments
defineArguments = Arguments
      <$> switch
          ( short 'r'
         <> help "Program takes regular expression from input, transform to internal representation.\n Afterwards it prints the same regular expression on standard output." )
      <*> switch
          ( short 't'
         <> help "Program takes regular expression from input and transforms the input to a finite automata." )
      <*> strArgument
          ( metavar "INPUT"
          <> value "" 
          <> help "Path to a input file with regular expression." )

-- Based of arguments function takes input and send it to main program 
resolveArguments :: Arguments -> IO ()
resolveArguments (Arguments False False _) = printErrorAndQuit "Wrong arguments!"
resolveArguments (Arguments r t []) = do line <- getLine
                                         program $ Arguments r t line
resolveArguments (Arguments r t i) = do file <- catch (readFile i) handleException
                                        program $ Arguments r t ( head $ lines file )

-- Handles exception when file couldn't be opened.
handleException :: SomeException -> IO String
handleException _ = printErrorAndQuit "Input file couldn't be opened!"

-- Prints error in form of plain text
printErrorAndQuit :: String -> IO a 
printErrorAndQuit m = putStr m >> exitWith (ExitFailure 1)                                   

-- Start of program
-- Main function calls argument parser and then calls function for handling 
-- and resolving received arguments
main :: IO ()
main = resolveArguments =<< execParser arguments
  where
    arguments = info (defineArguments <**> helper)
      ( fullDesc
     <> progDesc "FLP Project 2017/2018 - XKALOU03"
     <> header "rv-2-rka" )