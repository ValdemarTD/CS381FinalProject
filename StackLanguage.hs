-- StackLanguage.hs

module StackLanguage where

import System.Environment
import GHC.Types (IO (..))
import Data.Char
import Data.Bool
import Data.List
import System.IO
import Control.Monad
import System.IO.Unsafe -- IO was never explained in class so I will do this.
import Data.List.Split -- Install with cabal

---------------------
--    Constants    --
---------------------
whitespace = [' ', '\t', '\n', '\r']
newline = ['\n']
-- truthful = [0, ""] -- These are logical "true"
                      -- For non-bool primatives

---------------------
-- Abstract Syntax --
---------------------
-- Data
-- Haskell defined: Int, Bool, String
data Primitive = Int Int | Bool Bool | String String
    deriving (Eq, Show)

-- Control
-- An instruction which defines how an expression will evaluate
data Instr = Nop -- No operation
    | Hold
    | Release
    | Lbl
    | End
    | Inv
    
    -- IO
    | FileIn -- Parses the previous item on the stack
    | FileOut -- Writes the item two items ago to the fs using the filename
              -- at the previous item.
    | Echo -- Writes the previous item on the stack, then pops.
    
    
    -- The following stack operations are from forth.com
    | Swap -- Swaps the previous two items on the stack
    | Dup -- Duplicates the top item on the stack
    | Over -- Duplicates the item two items back and puts it on top
    | Rot -- Rotates the previous three items right
    | Drop -- Drops the top item of the stack
    deriving(Show)

data Expr = Raw Instr -- Instruction by itself
    | Flagged Instr Bool Bool Bool -- Modified instruction (skip flag) (nopop self flag) (nopop operands flag)
    deriving(Show)

-- The stack is a linked list
data Stack = Node Stack (Either Primitive Expr) Stack
    | Bottom
    | Top
    deriving(Show)

-- These instructions are pushed one at a time to the stack and then evaluated.
data ProgItem = Expr Expr
    | Val Primitive
    deriving(Show)

type Prog = [ProgItem]
    --deriving(Show)


-- Parsing

-- ...

---------------------
--     Backend     --
---------------------
-- Backend functions aren't explicitly part of the language.
-- They should never be called by the language.
-- They are just for loading scripts and stuff



-- Converts a list of words into a program
-- Currently case sensitive; needs to be made _not_ case sensitive.
-- TODO: Add a case allowing escape sequences to avoid commands.
-- TODO: Allow escape characters for string literals
-- TODO: Add a case to use flagged commands, eg in the following doctest:
-- 
-- >>> wordsToProg ["Echo&;"]
-- (Flagged Echo True True False) 
--
wordsToProg :: [String] -> Prog
wordsToProg ("ECHO":rest) = [Expr (Raw Echo)] ++ (wordsToProg rest)
wordsToProg (str:rest) = [Val (String str)] ++ (wordsToProg rest)
wordsToProg [] = [] -- Base case: all words consumed

splitStrsToProg :: [String] -> Prog
splitStrsToProg (a:b:xs) = (wordsToProg (words a)) ++ (wordsToProg [b])++ (splitStrsToProg xs)
-- The following two patterns should never be matched
splitStrsToProg (illegalLeft:illegalRight) = (strToProg illegalLeft) ++ (splitStrsToProg illegalRight)
splitStrsToProg other = (wordsToProg other)


-- Converts a string spanning several lines (and commands) to 
-- Haskell "Control" data types.
strToProg :: (String) -> Prog
strToProg "" = []
strToProg other = case (splitOn "\"" other) of
    (a:b:xs) ->  (strToProg a) ++ (wordsToProg [b]) ++ (splitStrsToProg xs)
    (b:xs) ->  wordsToProg (words b) ++ (splitStrsToProg xs)
    --[other_other] -> wordsToProg (words other_other)

-- Reads a file and "compiles" it as a StackLang program
-- TODO: Using unsafePerformIO is very bad practice in Haskell.
-- However, I will do it for now after getting frustrated with IO
-- for about an hour. If somebody changes this, it will probably
-- save some time on the final.
loadFile :: String -> Prog
loadFile filename = strToProg (unsafePerformIO . readFile $ filename)
        
    --(IO error) -> []
    -- Add error patterns here, example:
    -- IO Error -> Exec 0 [Nop] 

---------------------
--     Macros      --
---------------------
-- These commands, when read, expand to other commands.


---------------------
--    Semantics    --
---------------------

-- Runs an entire program until there are no instructions left.


-- Arithmetic --
-- Arithmetic is not always communicative, even when it is with basic math.
-- All three primitives are supported by all basic aritmetic operations
-- Specific relationships between primitives 

-- | This function adds two primitives together as per the language 
-- specifications in the design document
--
-- Examples:
-- 
-- >>> addPrimitives (Int 2) (Int 4)
-- (Int 6)
--
-- >>> addPrimitives (Int 2) (Bool True)
-- (Int 3)
--
-- >>> addPrimitives (Int 2) (String "nonEmpty")
-- (Int 3)
--
-- >>> addPrimitives (Int 2) (String "")
-- (Int 2)
-- 
-- Note: We use the traditional "0 = true" in this language.
--
-- >>> addPrimitives (Bool False) (Int 0)
-- (Bool True)
--
-- >>> addPrimitives (Bool False) (Bool True)
-- (Bool True)
--
-- >>> addPrimitives (Bool False) (Bool True)
-- (Bool True)
--
-- >>> addPrimitives (String "See: ") (Int 39)
-- (String "See: 39")
--
-- >>> addPrimitives (String "Logical True: ") (Bool True)
-- (String "Logical True: True")
--
-- >>> addPrimitives (String "abc") (String "def")
-- (String "abcdef")
--
addPrimitives :: Primitive -> Primitive -> Primitive
addPrimitives (Int x) (Int y) = undefined
addPrimitives (Int x) (Bool y) = undefined
addPrimitives (Int x) (String y) = undefined
addPrimitives (Bool x) (Int y) = undefined
addPrimitives (Bool x) (Bool y) = undefined
addPrimitives (Bool x) (String y) = undefined
addPrimitives (String x) (Int y) = undefined
addPrimitives (String x) (Bool y) = undefined
addPrimitives (String x) (String y) = undefined

-- | This function subtracts two primitives together as per the language 
-- specifications in the design document
--
-- Examples:
-- 
-- >>> subtractPrimitives (Int 2) (Int 4)
-- (Int -2)
--
-- >>> subtractPrimitives (Int 3) (Bool True)
-- (Int 2)
--
-- >>> subtractPrimitives (Int 7) (String "aaa")
-- (Int 4)
--
-- >>> subtractPrimitives (Bool True) (Int 17)
-- (Bool false)
--
-- >>> subtractPrimitives (Bool True) (Bool False)
-- (Bool False)
--
-- >>> subtractPrimitives (Bool True) (String "notEmpty")
-- (Bool False)
--
-- >>> subtractPrimitives (String "foobar") (Int 3)
-- (String "foo")
--
-- >>> subtractPrimitives (String "foobar \t") (Bool False)
-- (String "foobar")
--
-- >>> subtractPrimitives (String "foobar") (String "o")
-- (String "fbar")
--

subtractPrimitives :: Primitive -> Primitive -> Primitive
subtractPrimitives (Int x) (Int y) = undefined
subtractPrimitives (Int x) (Bool y) = undefined
subtractPrimitives (Int x) (String y) = undefined
subtractPrimitives (Bool x) (Int y) = undefined
subtractPrimitives (Bool x) (Bool y) = undefined
subtractPrimitives (Bool x) (String y) = undefined
subtractPrimitives (String x) (Int y) = undefined
subtractPrimitives (String x) (Bool y) = undefined
subtractPrimitives (String x) (String y) = undefined

-- | This function multiplies two primitives together as per the language 
-- specifications in the design document
--
-- Examples:
-- 
-- >>> multiplyPrimitives (Int 3) (Int 4)
-- (Int 12)
-- 
-- >>> multiplyPrimitives (Int 3) (String "asdf")
-- (Int 12)
-- 
-- >>> multiplyPrimitives (Int 3) (String "10")
-- (Int 30)
--
-- >>> multiplyPrimitives (Int 3) (Bool False)
-- (Int 3)
-- 
-- >>> multiplyPrimitives (Int 3) (Bool True)
-- (Int 0)
-- 
-- >>> multiplyPrimitives (Bool True) (Int 0)
-- (Bool False)
-- 
-- >>> multiplyPrimitives (Bool True) (Bool True)
-- (Bool False)
-- 
-- >>> multiplyPrimitives (Bool True) (String "")
-- (Bool False)
--
-- >>> multiplyPrimitives (String "foobar") (Int 2)
-- (String "foobarfoobar")
-- 
-- >>> multiplyPrimitives (String "f o o\tb a\tr") (Bool True)
-- (String "foobar")
-- 
-- >>> multiplyPrimitives (String "abc") (String "def")
-- (String "adbecf")
-- 

multiplyPrimitives :: Primitive -> Primitive -> Primitive
multiplyPrimitives (Int x) (Int y) = undefined
multiplyPrimitives (Int x) (Bool y) = undefined
multiplyPrimitives (Int x) (String y) = undefined
multiplyPrimitives (Bool x) (Int y) = undefined
multiplyPrimitives (Bool x) (Bool y) = undefined
multiplyPrimitives (Bool x) (String y) = undefined
multiplyPrimitives (String x) (Int y) = undefined
multiplyPrimitives (String x) (Bool y) = undefined
multiplyPrimitives (String x) (String y) = undefined
