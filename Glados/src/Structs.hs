module Structs (
  Env,
  Operator(..),
  AstValue(..)
) where

type Env = [(String, AstValue)]

type Stack = [Value]

type Insts = [Instruction]

type Args = [Value]

type EnvVm = [(String, Value)]

type FuncInsts = [Insts]

type ListArgs = [Args]

data Operator =
      Add
    | Sub
    | Mul
    | Div
    | Eq
    | Ne
    | Gt
    | Lt
    | Le
    | Ge
    | And
    | Or
    | Not
    deriving (Show, Eq)

data AstValue =
      AstNumber Int
    | AstLiteral String
    | AstBoolean Bool
    | AstSymbol String
    | AstList [AstValue]
    | AstArgList [AstValue]
    | AstBody [AstValue]
    | AstDefine AstValue AstValue
    | AstCond AstValue AstValue AstValue
    | AstCall AstValue AstValue
    | AstFunc AstValue AstValue AstValue
    | AstBinaryOp Operator AstValue AstValue
    | AstError String Int
    deriving (Show, Eq)

data Value =
      Numerical Int
    | Boolean Bool
    | Operator Operator
    | Insts [Instruction]
    | Function Insts
    deriving (Show)

data Instruction =
      Push Value
    | Pop
    | Call
    | Ret
    | JumpIfFalse Int
    | PushArg Int
    | PushEnv String
    deriving (Show)
