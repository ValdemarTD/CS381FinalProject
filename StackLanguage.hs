-- StackLanguage.hs

module StackLanguage where

import System.Environment
import GHC.Types (IO (..))
import Data.Char

---------------------
-- Abstract Syntax --
---------------------
-- Data
-- Haskell defined: Int, Bool, String
data Val = Int Int | Bool Bool | String String
    deriving (Eq, Show)

-- Control
-- An expression which evaluates using subsequent items
-- on the stack
data Expr = Nop -- No operation
    | Add -- Add ints, bools, and strings (even interoperable- think concatenation)
    | Sub -- See above
    | Mul -- blah blah blah
    | FileIn -- Parses the previous item on the stack
    | FileOut -- Writes the item two items ago to the stack using the filename
              -- at the previous item. e.g. 3 "foo.txt" FileOut writes 3 to 
              -- foo.txt. Then pops.
    | Dump -- Writes the whole stack to a file. Then pops.
    | Echo -- Writes the previous item on the stack, then pops.
    | BranchIfTrue -- Jumps "pc-1" items along the stack if "pc-2" is true.
    | BranchIfFalse -- What it says on the tin
    
    -- The following stack operations are from forth.com
    | Swap -- Swaps the previous two items on the stack
    | Dup -- Duplicates the top item on the stack
    | Over -- Duplicates the item two items back and puts it on top
    | Rot -- Rotates the previous three items right
    | Drop -- Drops the top item of the stack
    deriving(Show)
    

-- The stack is a linked list
data Stack = Node Val Stack
    | End
    deriving(Show)

-- These instructions are pushed one at a time to the stack and then evaluated.
data ProgItem = Expr Expr
    | Val Val
    | Function Prog
    deriving(Show)

data Prog = Prog [ProgItem]
    deriving(Show)


-- Parsing

-- ...

---------------------
--     Backend     --
---------------------
-- Backend functions aren't explicitly part of the language.
-- They should never be called by the language.
-- They are just for loading scripts and stuff

-- Converts a string spanning several lines (and commands) to 
-- Haskell "Control" data types.
strToProg = undefined

-- Reads a file and "compiles" it as a StackLang program
loadFile :: String -> Prog
loadFile filename = case (readFile filename) of
    IO str -> strToProg str
    -- Add error patterns here, example:
    -- IO Error -> Exec 0 [Nop] 

---------------------
--    Semantics    --
---------------------

-- Runs an entire program until there are no instructions left.
run :: Prog -> Stack -> Prog
run (Prog []) _ = Prog [] -- End state.
run (Prog (x:xs)) oldStack = case x of
    (Expr progExpression) -> (run (Prog xs) (doInstruction progExpression oldStack))
    (Val progVal) -> (run (Prog xs) (Node progVal oldStack))

-- Performs an expression on the stack
doInstruction :: Expr -> Stack -> Stack
doInstruction Swap (Node lhs (Node rhs oldStack)) = (Node rhs (Node lhs oldStack))
doInstruction Swap (Node lhs End) = undefined
-- So on and so forth
