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

data Instr = Label T.Text | Ret | Halt | Link Int | Unlink | Adjust Int
           | StoreReg Register | LoadReg Register
           | LoadLocal Int | StoreLocal Int
           | LoadConst Integer
           | BranchSubr T.Text | BranchAlways T.Text | BranchFalse T.Text
           | NotOp | AndOp | OrOp | XorOp
           | EqOp | NeOp | LtOp | LeOp | GtOp | GeOp
           | NegOp | AddOp | SubOp | MulOp | DivOp | ModOp
           | TrapInt | TrapChar

tab :: String
tab = "  "

instance Show Instr where
  show (Label t) = T.unpack t <> ":"
  show Ret = tab <> "ret"
  show Halt = tab <> "halt"
  show (Link i) = tab <> "link " <> show i
  show Unlink = tab <> "unlink"
  show (Adjust i) = tab <> "ajs " <> show i

  show (StoreReg r) = tab <> "str " <> show r
  show (LoadReg r) = tab <> "ldr " <> show r
  show (LoadLocal i) = tab <> "ldl " <> show i
  show (StoreLocal i) = tab <> "stl " <> show i

  show (LoadConst i) = tab <> "ldc " <> show i

  show (BranchSubr t) = tab <> "bsr " <> T.unpack t
  show (BranchAlways t) = tab <> "bra " <> T.unpack t
  show (BranchFalse t) = tab <> "brf " <> T.unpack t

  show NotOp = tab <> "not"
  show AndOp = tab <> "and"
  show OrOp = tab <> "or"
  show XorOp = tab <> "xor"

  show EqOp = tab <> "eq"
  show NeOp = tab <> "ne"
  show LtOp = tab <> "lt"
  show LeOp = tab <> "le"
  show GtOp = tab <> "gt"
  show GeOp = tab <> "ge"

  show NegOp = tab <> "neg"
  show AddOp = tab <> "add"
  show SubOp = tab <> "sub"
  show MulOp = tab <> "mul"
  show DivOp = tab <> "div"
  show ModOp = tab <> "mod"

  show TrapInt = tab <> "trap 0"
  show TrapChar = tab <> "trap 1"
