module CodeGen.Instructions (
  Register(..), Instr(..)
) where

import qualified Data.Text as T

data Register = ProgCounter | StackPointer | MarkPointer | HeapPointer
              | RetReg | Scratch1 | Scratch2 | Scratch3

instance Show Register where
  show ProgCounter = "PC"
  show StackPointer = "SP"
  show MarkPointer = "MP"
  show HeapPointer = "HP"
  show RetReg = "RR"
  show Scratch1 = "R5"
  show Scratch2 = "R6"
  show Scratch3 = "R7"

data Instr = Label T.Text | Ret | Halt | Link Int | Unlink
           | StoreReg Register | LoadReg Register
           | LoadLocal Int | StoreLocal Int
           | LoadConst Integer
           | BranchSubr T.Text | BranchAlways T.Text | BranchFalse T.Text
           | NotOp | AndOp | OrOp | XorOp
           | EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
           | NegOp | AddOp | SubOp | MulOp | DivOp | ModOp
           | TrapInt | TrapChar

instance Show Instr where
  show (Label t) = T.unpack t <> ":"
  show Ret = "ret"
  show Halt = "halt"
  show (Link i) = "link " <> show i
  show Unlink = "unlink"

  show (StoreReg r) = "str " <> show r
  show (LoadReg r) = "ldr " <> show r
  show (LoadLocal i) = "ldl " <> show i
  show (StoreLocal i) = "stl " <> show i

  show (LoadConst i) = "ldc " <> show i

  show (BranchSubr t) = "bsr " <> T.unpack t
  show (BranchAlways t) = "bra " <> T.unpack t
  show (BranchFalse t) = "brf " <> T.unpack t

  show NotOp = "not"
  show AndOp = "and"
  show OrOp = "or"
  show XorOp = "xor"

  show EqOp = "eq"
  show NeOp = "ne"
  show LtOp = "lt"
  show LeOp = "le"
  show GtOp = "gt"
  show GeOp = "ge"

  show NegOp = "neg"
  show AddOp = "add"
  show SubOp = "sub"
  show MulOp = "mul"
  show DivOp = "div"
  show ModOp = "mod"

  show TrapInt = "trap 0"
  show TrapChar = "trap 1"
