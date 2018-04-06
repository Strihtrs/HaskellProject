-- rv-2-rka
-- Filip Kalous
-- xkalou03


-- This module contains definitions of various data types and theirs Show instances and aliases.
-- These definitions are used for implementing main haskell program in file rv-2-rka.hs.
module Types where

    import Data.List

    -- Data type for storing program arguments
    data Arguments = Arguments
        { 
            toIntRepr   :: Bool,
            toAutomata  :: Bool,
            inputString   :: String 
        }

    -- Data type representing tree.
    -- which is used for storing regular expression to its internal representation.
    data Tree 
            = Symbol Char
            | Operation Char Tree Tree
            | Star Char Tree 
            deriving Show

    -- Data type representing transition in finite automata.
    -- For example: (1 --a--> 2) - Transition from state 1 to state 2 using 'a' like a transition.
    data TTransition = TTransition 
        {
            left  :: TState,
            right :: TState,
            transition :: TTLabel
        }

    -- Data type representing finite automata.
    data FSM = FSM 
        {
            states      :: [TState],
            init        :: TState,
            end         :: [TState],
            transitions :: [TTransition]
        }

    -- Data type representing label of a transition.
    -- Epsilon is a special constructor for empty transition.
    data TTLabel = TTLabel Char | Epsilon

    -- Alias for state of finite automata
    type TState = Int

    -- Alias for array of trees
    type TTree = [Tree]

    -- Instance of Show for custom type TTLabel
    -- Output format: character or empty string
    instance Show TTLabel where
        show (TTLabel '@') = show Epsilon
        show (TTLabel a) = [a]
        show Epsilon = ""

    -- Instance of Show for custom type TTransition
    -- Output format: 1,a,2 for transition 1 --a--> 2
    instance Show TTransition where
        show (TTransition l r t) = show l 
                                    ++ "," 
                                    ++ show t 
                                    ++ "," 
                                    ++ show r

    -- Instance of Show for custom type FSM
    -- This instance shows on output equivalent finite automata for input regular expression
    instance Show FSM where
        show (FSM s i e t) = intercalate "\n" [printStates s, show i, printFinalStates e, printTransitions t]
                                where
                                      printStates      s = intercalate "," $ map show s
                                      printFinalStates e = intercalate "," $ map show e
                                      printTransitions t = intercalate "\n" $ map show t