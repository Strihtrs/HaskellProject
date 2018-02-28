import Data.List
import Data.Bool
import Control.Monad
import System.IO
import Options.Applicative
import Data.Semigroup ((<>))

-- Obecny strom
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving (Show)
-- Derivacny strom
type DTree = Tree Char
-- Stav RKA je pre jednoduchost int
type RKAstate = Int
-- Regulárny Konečný Automat
data RKA = RKAnone
         | RKA { states :: [RKAstate]
               , trans  :: [RKAtrans]
               , start  ::  RKAstate
               , ends   :: [RKAstate] }
-- Prechod medzi dvoma stavmi
data RKAtrans = RKAtrans
    { src :: RKAstate
    , sym :: Char
    , dst :: RKAstate
    } deriving (Eq)

instance Show RKAtrans where
    show (RKAtrans a ' ' b) = show a ++ ",," ++ show b
    show (RKAtrans a s b) = intercalate "," [show a, [s], show b]

instance Show RKA where
    show RKAnone = []
    show (RKA states trans start ends)
        = lines [csv states, show start, csv ends, lines $ map show trans]
        where
            csv = intercalate "," . map show 
            lines = intercalate "\n"

rv2tree :: String -> Either String DTree
rv2tree s = case rv2tree' s of
    [x] -> Right x
    _   -> Left "Invalid regular expression"
    where 
        rv2tree' = foldl insert []
        insert (x:y:ys) '+' = Node y '+' x    : ys
        insert (x:y:ys) '.' = Node y '.' x    : ys
        insert (x:xs)   '*' = Node x '*' Leaf : xs
        insert xs value
          | all (value/=) "+*. " = Node Leaf value Leaf : xs
          | otherwise = []

tree2rv :: DTree -> String
tree2rv = reverse . tree2rv''
    where
        tree2rv'' Leaf = []
        tree2rv'' (Node Leaf sym Leaf)  = [sym]
        tree2rv'' (Node left sym Leaf)  = sym : tree2rv'' left
        tree2rv'' (Node left sym right) = sym : tree2rv'' right ++ tree2rv'' left

tree2rkaString :: Either String DTree -> String
tree2rkaString (Left  e) = e
tree2rkaString (Right t) = show $ tree2rka t

tree2rka :: DTree -> RKA
tree2rka Leaf = RKAnone
tree2rka (Node left sym right)
    | sym == '*' = expandITR l
    | sym == '+' = expandOR  l r
    | sym == '.' = expandAND l r
    | otherwise  = RKA [1, 2] [RKAtrans 1 sym 2] 1 [2]
    where
        l = tree2rka left
        r = tree2rka right

expandAND (RKA statesA transA startA endsA)
          (RKA statesB transB startB endsB)
          = RKA statesN transN startA endsN
          where
            offset  = length statesA + 1
            statesN = statesA ++ [stateC] ++ map (offset+) statesB -- States from first machine + connecting state + offset states from second machine
            stateC  = offset;                                      -- Connecting state
            transN  = transA                                       -- First machine's transitions
                      ++ route endsA [stateC]                      -- First machine's end to connecting state
                      ++ route [stateC] [startB + offset]          -- Connecting state to second machine's start
                      ++ offsetTrans offset transB                 -- Offset second machine's transitions
            endsN   = map (offset+) endsB
{-
        ->(1)-a>((2))
              +
        ->(1)-b>((2))
              =
->(1)-a>(2)-ε>(3)-ε>(4)-b>((5))
-}
---------------------------------------------------------------------------------------------------------------------------------------------
expandOR  (RKA statesA transA startA endsA)
          (RKA statesB transB startB endsB)
          = RKA statesN transN startN endsN
          where
            offset  = length statesA + 1
            startN  = 1
            statesN = startN : map (1+) statesA ++ map (offset+) statesB ++ endsN -- Add new start state and new end state
            transN  = route [1] [startA + 1, startB + offset]                     -- New start to original starts
                      ++ offsetTrans 1 transA                                     -- Offset first machine's transitions by one
                      ++ offsetTrans offset transB                                -- Offset second machine's transitions by |first machine| + 1
                      ++ route (map (1+) endsA ++ map (offset+) endsB) endsN      -- Connect original ends to new end
            endsN   = [offset + length statesB + 1]
{-
    ->(1)-a>((2))
          +
    ->(1)-b>((2))
          =
      (2)-a>(3)
     /^ε      ε\
->(1)           ((6))
     \ε       ε/^
      (4)-b>(5)
-}
-------------------------------------------------------------------------------------------------------
expandITR (RKA states trans start ends)
          = RKA statesN transN startN endsN
          where
            startN  = 1
            statesN = startN : map (1+) states ++ endsN -- Add new start state and new end state
            transN  = route [1] (start+1:endsN)         -- New start to original start and new end
                      ++ offsetTrans 1 trans            -- Offset existing transitions by 1
                      ++ route (map (1+) ends) endsN    -- Connect original ends to the new end
                      ++ route endsN [startN]           -- Connect new end to new start
            endsN = [length states + 2]
{-
      ->(1)-a>((2))
            *
            =
    /"""""""ε"""""""^\
->(1)-ε>(2)-a>(3)-ε>((4))
    \_______ε________/^
-}
------------------------------------------------------------------
route :: [RKAstate] -> [RKAstate] -> [RKAtrans]
route srcs dsts = [RKAtrans src ' ' dst | src <- srcs, dst <- dsts]

offsetTrans :: Int -> [RKAtrans] -> [RKAtrans]
offsetTrans num = map offsetTrans'
    where offsetTrans' (RKAtrans src sym dst) = RKAtrans (src + num) sym (dst + num)
------------------------------------------------------------------------------------
data Args = Args { isRead :: Bool, isTransform :: Bool, vstup :: String }

main :: IO ()
main = do
    rv2rka =<< execParser args
        where
            args = info (readArgs <**> helper)
             ( fullDesc
             <> header "rv-2-rka"
             <> progDesc "XJASKA00 - Projekt pre FLP 2017" )

rv2rka :: Args -> IO ()
rv2rka (Args r t []) = do input <- getLine
                          rv2rka' $ Args r t input
rv2rka (Args r t f)  = do file <- readFile f
                          rv2rka' $ Args r t (head $ lines file)

rv2rka' :: Args -> IO ()
rv2rka' (Args r t s)
    | t = do putStr $ tree2rkaString $ rv2tree s
    | r = do putStr $ getInternalRep $ rv2tree s
    | otherwise = putStrLn "Invalid arguments"

getInternalRep :: Either String DTree -> String
getInternalRep (Left e) = e
getInternalRep (Right t) = "Derivation Tree:\n" ++ show t ++ "\nBack to regex:\n" ++ tree2rv t

readArgs :: Parser Args
readArgs = Args
    <$> switch
         ( short 'r'
        <> help "Načte vstup a vypíše vnitřní reprezentaci" )
    <*> switch
         ( short 't'
        <> help "Převede na RKA" )
    <*> strArgument
         (
         metavar "VSTUP"
        <> value "" )