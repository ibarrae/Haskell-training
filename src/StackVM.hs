module StackVM (StackVal(..), StackExp(..), Stack, Program, stackVM) where

-- Values that may appear in the stack. Such a value will also be
-- returned by the stackVM program execution function.
data StackVal = IVal Integer | BVal Bool | Void deriving Show

-- The various expressions our VM understands.
data StackExp = PushI Integer
                | PushB Bool
                | AddVM
                | MulVM
                | And
                | Or
                deriving (Show, Eq)

type Stack   = [StackVal]
type Program = [StackExp]

-- Execute the given program. Returns either an error message or the
-- value on top of the stack after execution.
stackVM :: Program -> Either String StackVal
stackVM = execute []

errType :: String -> Either String a
errType op = Left $ "Encountered '" ++ op ++ "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op = Left $ "Stack underflow with '" ++ op ++ "' opcode."

-- Execute a program against a given stack.
execute :: Stack -> Program -> Either String StackVal
execute [] []                               = Right Void
execute (s:_) []                            = Right s

execute s (PushI x : xs)                    = execute (IVal x : s) xs
execute s (PushB x : xs)                    = execute (BVal x : s) xs

execute (IVal s1 : IVal s2 : ss) (AddVM : xs) = execute (s':ss) xs
    where s' = IVal (s1 + s2)
execute (_:_:_) (AddVM:_)                     = errType "AddVM"
execute _ (AddVM:_)                           = errUnderflow "AddVM"

execute (IVal s1:IVal s2:ss) (MulVM : xs)     = execute (s':ss) xs
    where s' = IVal (s1 * s2)
execute (_:_:_) (MulVM:_)                     = errType "MulVM"
execute _ (MulVM:_)                           = errUnderflow "MulVM"

execute (BVal s1:BVal s2:ss) (And : xs)     = execute (s':ss) xs
    where s' = BVal (s1 && s2)
execute (_:_:_) (And:_)                     = errType "And"
execute _ (And:_)                           = errUnderflow "And"

execute (BVal s1 : BVal s2 : ss) (Or : xs)  = execute (s':ss) xs
    where s' = BVal (s1 || s2)
execute (_:_:_) (Or:_)                      = errType "Or"
execute _ (Or:_)                            = errUnderflow "Or"