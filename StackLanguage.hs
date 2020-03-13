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
import Debug.Trace -- Try to replace this with IO later.

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
    | Add
    | Sub
    | Mul
    | Eq
    | Set
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

-- The stack is a doubly linked list behind the scenes, but
-- this *shouldn't* be exposed to the language programmer.
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

wordsToProg s = case s of
                ("ECHO":rest) -> [Expr (Raw Echo)] ++ (wordsToProg rest)
                ("ADD":rest) -> [Expr (Raw Add)] ++ (wordsToProg rest)
                ("FILEIN":rest) -> [Expr (Raw FileIn)] ++ (wordsToProg rest)
                ("FILEOUT":rest) -> [Expr (Raw FileOut)] ++ (wordsToProg rest)
                ("+":rest) -> [Expr (Raw Add)] ++ (wordsToProg rest)
                ("-":rest) -> [Expr (Raw Sub)] ++ (wordsToProg rest)
                ("*":rest) -> [Expr (Raw Mul)] ++ (wordsToProg rest)
                ("==":rest) -> [Expr (Raw Eq)] ++ (wordsToProg rest)
                ("=":rest) -> [Expr (Raw Set)] ++ (wordsToProg rest)
                ("SWAP":rest) -> [Expr (Raw Swap)] ++ (wordsToProg rest)
                ("DUP":rest) -> [Expr (Raw Dup)] ++ (wordsToProg rest)
                ("OVER":rest) -> [Expr (Raw Over)] ++ (wordsToProg rest)
                ("ROT":rest) -> [Expr (Raw Rot)] ++ (wordsToProg rest)
                ("DROP":rest) -> [Expr (Raw Drop)] ++ (wordsToProg rest)
                ("True":rest) -> [Val (Bool True)] ++ (wordsToProg rest)
                ("False":rest) -> [Val (Bool False)] ++ (wordsToProg rest)
                ((f:str):rest) -> case (isDigit f) of
                                (True) -> [Val (Int (read ([f] ++ str) :: Int))] ++ (wordsToProg rest)
                                (False) -> case f of
                                          '-' ->  [Val (Int (read ([f] ++ str) :: Int))] ++ (wordsToProg rest)
                                          _ -> [Val (String ([f] ++ str))] ++ (wordsToProg rest)
                (str:rest) -> [Val (String str)] ++ (wordsToProg rest)
                [] -> [] -- Base case: all words consumed


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

-- Runs a statement in a program
statement :: Expr -> Primitive -> Primitive -> Primitive
statement ex v1 v2 = case ex of
                                        (Raw Add) -> addPrimitives v1 v2
                                        (Raw Sub) -> subtractPrimitives v1 v2
                                        (Raw Mul) -> multiplyPrimitives v1 v2
                                        (Raw Echo) -> String (show v1) -- this line threw an error

encapsulated_print :: Primitive -> IO()
encapsulated_print v = print v

nop :: Stack -> Stack
nop s = s

-- This version of echo_cmd is popping; it removes what it prints from the stack.
echo_cmd :: Stack -> Stack
echo_cmd (Node lhs val rhs) = case val of
    Left p -> lhs -- You can't use print here or it taints the program with the IO monad
    Right ex -> (Node lhs val rhs) -- This is an error

-- This is an example of what the semantic wrapper for an aritmetic operation
-- would look like. Similar functions could be written for subtraction and 
-- multiplication givenwh
add_cmd :: Stack -> Stack
add_cmd (Node lhs (Left op1) (Node _ (Left op2) rhs)) =
    (Node lhs (Left (addPrimitives op1 op2)) rhs)
-- It is an error if this line is reached, because the stack did not have the necessary operands.
add_cmd error_stack = error_stack

-- Pushes an item to a stack
push :: Primitive -> Stack -> Stack
push toAdd prev = case prev of
    (Node lhs v rhs) -> (Node lhs v (Node prev (Left toAdd) rhs))
    _ -> (Node Bottom (Left toAdd) Top)
-- Unfortunately, we couldn't figure out how to make a "reference" to the left hand
-- side without simply copy-pasting the values

formatStack :: Stack -> IO ()
formatStack s = print s

-- Runs an entire program until there are no instructions left.
run :: Prog -> IO ()
run p = consume p (Node Bottom (Left (Int (-1))) Top)

consume :: Prog -> Stack -> IO ()
consume (x:xs) (Node lhs val rhs) = case x of
    -- Expressions and outcomes --
    Expr (Raw Echo) -> consume (xs) (echo_cmd (Node lhs val rhs))
    Expr (Raw Add) -> consume (xs) (add_cmd (Node lhs val rhs))
    -- We would add semantics for the rest of our program here.
    
    -- Primitives --
    Val value -> consume (xs) (push value (Node lhs val rhs))
    -- if the following line runs, it means that the command
    -- has not been implemented
    _ -> consume (xs) (Node lhs val rhs)
    
consume [] s = formatStack s -- End of program




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

addPrimitives (Int x) y = case y of
                          (Int v) -> (Int (x + v))
                          (Bool v) -> case v of
                                      (True) -> (Int (x + 1))
                                      (False) -> (Int x)
                          (String v) -> case v of
                                        "" -> (Int x)
                                        _ -> (Int (x + 1))

addPrimitives (Bool x) y = case y of
                           (Int v) -> case v of
                                      0 -> (Bool True)
                                      _ -> (Bool x)
                           (Bool v) -> (Bool (x && v))
                           (String v) -> case v of
                                         "" -> (Bool True)
                                         _ -> (Bool x)

addPrimitives (String x) y = case y of
                             (Int v) -> (String (x ++ (show v)))
                             (Bool v) -> case v of
                                         (True) -> (String (x ++ "True"))
                                         (False) -> (String (x ++ "False"))
                             (String v) -> (String (x ++ v))


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

subtractPrimitives (Int x) y = case y of
                               (Int v) -> (Int (x - v))
                               (Bool v) -> case v of
                                           (True) -> (Int (x - 1))
                                           (False) -> (Int x)
                               (String v) -> (Int (x - (length v)))

subtractPrimitives (Bool x) y = case y of
                                (Int v) -> case v of
                                           0 -> (Bool (x && True))
                                           _ -> (Bool False)
                                (Bool v) -> (Bool (x && v))
                                (String v) -> case v of
                                              "" -> (Bool (x && True))
                                              _ -> (Bool False)

subtractPrimitives (String x) y = case y of
                                  (Int v) -> case x of
                                             (s:str) -> case (v < (length x)) of
                                                        (True) -> (String ([s] ++ show (subtractPrimitives (String str) (Int v))))
                                                        (False) -> (String [])
                                  (String v) -> case x of
                                                [] -> (String [])
                                                (front:mid:end) -> case (v == [mid]) of
                                                                   (True) -> (String ([front] ++ show (subtractPrimitives (String end) (String v))))
                                                                   (False) -> case x of
                                                                              (start:rest) -> case (v == [start]) of
                                                                                   (True) -> (String (show (subtractPrimitives (String rest) y)))
                                                                                   (False) -> (String x)


--subtractPrimitives (String x) (Bool y) = undefined -- This line was throwing a warning


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

multiplyPrimitives (Int x) y = case y of
                               (Int v) -> (Int (v * x))
                               (Bool v) -> case v of
                                           (True) -> (Int 0)
                                           (False) -> (Int x)
                               (String v) -> (Int (x * (length v)))

multiplyPrimitives (Bool x) y = case y of
                                (Bool v) -> case v of
                                            (True) -> (Bool (not x))
                                            (False) -> (Bool x)
                                (Int v) -> case v of
                                           0 -> (Bool (not x))
                                           _ -> (Bool x)
                                (String v) -> case v of
                                              "" -> (Bool (not x))
                                              _ -> (Bool x)

multiplyPrimitives (String x) y = case y of
                                  (Int v) -> case v of
                                             0 -> (String [])
                                             _ -> (String (x ++ show (multiplyPrimitives (String x) (Int (v - 1)))))

                                  (Bool v) -> error "Multiplication of strings by bools is not currently supported"
                                  (String v) -> case x of
                                                [] -> (String [])
                                                _ -> (String v)
