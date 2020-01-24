{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day16
-- License     : BSD3

module AOC.Challenge.Day16 (
    day16a
  , day16b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Data.Bits

instruction = sepBy1 int (char ' ')

intArray :: ReadP [Int]
intArray = read <$> (many1 $ satisfy (/= '\n'))

sample = do
  string "Before: "
  before <- intArray
  string "\n"
  inst <- instruction
  string "\n"
  string "After: "
  after <- intArray
  return (inst, (before, after))

type Registers = [Int]
type Instruction = [Int]
type Parameters = [Int]
type Sample = (Instruction, (Registers, Registers))

data Mode = Register | Value | Ignore
type OpCode = (String, Mode, Mode, Int -> Int -> Int)

opcodes :: [OpCode]
opcodes = [
            ("addr", Register, Register, (+)),
            ("addi", Register, Value, (+)),
            ("mulr", Register, Register, (*)),
            ("muli", Register, Value, (*)),
            ("banr", Register, Register, (.&.)),
            ("bani", Register, Value, (.&.)),
            ("borr", Register, Register, (.|.)),
            ("bori", Register, Value, (.|.)),
            ("setr", Register, Ignore, (const)),
            ("seti", Value, Ignore, (const)),
            ("gtir", Value, Register, (\a b -> fromEnum $ a > b)),
            ("gtri", Register, Value, (\a b -> fromEnum $ a > b)),
            ("gtrr", Register, Register, (\a b -> fromEnum $ a > b)),
            ("eqir", Value, Register, (\a b -> fromEnum $ a == b)),
            ("eqri", Register, Value, (\a b -> fromEnum $ a == b)),
            ("eqrr", Register, Register, (\a b -> fromEnum $ a == b))
          ]

exec :: Registers -> OpCode -> Parameters -> Registers
exec regs (_, aMode, bMode, f) [a, b, c] = replace c result regs
  where result = f va vb
        va = case aMode of Register -> regs !! a ; Value -> a ; Ignore -> undefined
        vb = case bMode of Register -> regs !! b ; Value -> b ; Ignore -> undefined

behavesLike :: Sample -> OpCode -> Bool
behavesLike (instruction, (before, after)) op = after == exec before op (tail instruction)

matches :: Sample -> [OpCode]
matches sample = filter (behavesLike sample) opcodes

day16a :: [Sample] :~> Int
day16a = MkSol
    { sParse = parseMaybe $ sepBy1 sample (string "\n\n")
    , sShow  = show
    , sSolve = Just . length . filter (\sample -> length (matches sample) >= 3)
    }

day16b :: _ :~> _
day16b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
