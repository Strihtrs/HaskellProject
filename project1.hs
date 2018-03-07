import Options.Applicative
import Data.Semigroup ((<>))
import System.IO

data Arguments = Arguments
  { toIntRepr   :: Bool
  , toAutomata  :: Bool
  , inputString   :: String }

data TTree 
        = Symbol Char
        | Operation Char TTree TTree
        | Star Char TTree 
        deriving Show

type TState = Int

data TTransition = TTransition 
    {
        left  :: TState,
        right :: TState,
        transition :: Char
    }

data FSM = FSM 
    {
        states      :: [TState],
        init        :: TState,
        end         :: [TState],
        transitions :: [TTransition]
    }

type Tree = [TTree]

instance Show TTransition where
    show (TTransition l r t) = show l ++ "," ++ show t ++ "," ++ show r

instance Show FSM where
    show (FSM s i e t) = show s ++ "\n" ++ show i ++ "\n" ++ show e ++ "\n" ++ show t

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

step :: String -> Either String TTree
step input = case foldl parse [] input of
    [x] -> Right x
    _   -> Left "Regular expression is not valid!"
    where 
          parse (r:l:t) '.' = (Operation '.' l r):t
          parse (r:l:t) '+' = (Operation '+' l r):t
          parse (l:t)   '*' = (Star '*' l):t
          parse t char
            | char `notElem` ['*', '+', '.'] = (Symbol char):t
            | otherwise = []

resolveArguments :: Arguments -> IO ()
resolveArguments (Arguments r t []) = do line <- getLine
                                         program $ Arguments r t line

resolveArguments (Arguments r t i) = do file <- readFile i
                                        program $ Arguments r t ( head $ lines file )

tree2fsm :: TTree -> FSM
tree2fsm (Symbol a) = ( FSM [1, 2] 1 [2] [(TTransition 1 2 a)] )
tree2fsm (Star c t) = fsmStar ( tree2fsm t )
tree2fsm (Operation o l r)
    | o == '+' = fsmUnion  (tree2fsm l) (tree2fsm r)
    | o == '.' = fsmConcat (tree2fsm l) (tree2fsm r)

inc :: Int -> Int
inc a = 1 + a

incList :: [Int] -> Int -> [Int]
incList a i = map (+i) a

incTransitions :: [TTransition] -> [TTransition]
incTransitions ts = map (\(TTransition a b c) -> TTransition (inc a) (inc b) c) ts

-- Expand iteration
fsmStar :: FSM -> FSM
fsmStar (FSM sA iA eA tA) = FSM ([1] ++ getNewStates) 1 [getNewEndState] (getNewTransitions)
    where
          getNewStates      = (incList sA 1) ++ getNewEndState:[]
          getNewEndState    = 2 + length sA
          getNewTransitions = [TTransition 1 (inc iA) '@', TTransition 1 getNewEndState '@']
                              ++ incTransitions tA
                              ++ ( map (\s -> TTransition s getNewEndState '@') (incList eA 1) ) 
                              ++ [TTransition getNewEndState 1 '@']

-- Expand union
fsmUnion :: FSM -> FSM -> FSM
fsmUnion (FSM sA iA eA tA) (FSM sB iB eB tB) = FSM (getNewStates) 1 [getNewEndState] (getNewTransitions)
    where
          fsmAEnd           = (1 + length sA)
          getNewStates      = 1:[] ++ (incList sA 1) ++ (incList sB fsmAEnd) ++ [getNewEndState]
          getNewEndState    = 2 + ((length sA) + (length sB))
          getNewTransitions = [TTransition 1 (inc iA) '@', TTransition 1 (fsmAEnd + iB) '@']
                                ++ incTransitions tA
                                ++ ( map (\(TTransition a b c) -> TTransition (fsmAEnd + a) (fsmAEnd + b) c) tB )
                                ++ ( map (\s -> TTransition s getNewEndState '@') (incList eA 1) )
                                ++ ( map (\s -> TTransition s getNewEndState '@') (incList eB fsmAEnd) )

-- Expand concatenation
fsmConcat :: FSM -> FSM -> FSM
fsmConcat (FSM sA iA eA tA) (FSM sB iB eB tB) = FSM (getNewStates) iA (getNewEndStates) (getNewTransitions)
    where
          middleState       = (1 + length sA)
          getNewEndStates   = incList eB middleState
          getNewStates      = sA ++ [middleState] ++ ( incList sB middleState )
          getNewTransitions = tA 
                                ++ ( map (\s -> TTransition s middleState '@') eA )
                                ++ [TTransition middleState (iB + middleState) '@']
                                ++ ( map (\(TTransition a b c) -> TTransition (middleState + a) (middleState + b) c) tB )

printFSM :: Either String TTree -> String
printFSM (Right t) = show ( tree2fsm t )
printFSM (Left error) = error


printInternal :: Either String TTree -> String
printInternal (Right t) = printTree t
printInternal (Left error) = error

printTree :: TTree -> String
printTree (Symbol a) = a:[]
printTree (Star o t) = printTree t ++ o:[]
printTree (Operation o l r) = printTree l ++ printTree r ++ o:[]

program :: Arguments -> IO ()
program (Arguments r t i) 
    | r = do putStr $ printInternal $ step i
    | t = do putStr $ printFSM $ step i

main :: IO ()
main = resolveArguments =<< execParser arguments
  where
    arguments = info (defineArguments <**> helper)
      ( fullDesc
     <> progDesc "XKALOU03 - Project to FLP 2018 course"
     <> header "rv-2-rka" )