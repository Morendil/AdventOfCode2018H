{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day19
-- License     : BSD3

module AOC.Challenge.Day19 (
    day19a
  , day19b
  ) where

import AOC.Prelude

import Text.ParserCombinators.ReadP
import Data.Bits
import Data.List.Extra (nubOn)
import Data.List.HT (takeUntil)
import qualified Data.Map as M
import qualified Data.Set as S

instruction :: ReadP Instruction
instruction = do
  opName <- many1 $ satisfy (/= ' ')
  string " "
  params <- sepBy1 int (string " ")
  return (fromJust $ find (\op -> name op == opName) opcodes, params)

sourceCode :: ReadP Program
sourceCode = do
  string "#ip "
  ip <- int
  string "\n"
  instructions <- sepBy1 instruction (string "\n")
  return (ip, instructions)

type Registers = [Int]
type Parameters = [Int]
type Instruction = (OpCode, Parameters)
type Program = (Int, [Instruction])
type Memory = (Int, Registers)

data Mode = Register | Value | Ignore
type OpCode = (String, Mode, Mode, Int -> Int -> Int)

name :: OpCode -> String
name (v, _, _, _) = v

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
            ("setr", Register, Ignore, const),
            ("seti", Value, Ignore, const),
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
exec _ _ _ = error "Oops"

execInstruction :: Registers -> Instruction -> Registers
execInstruction regs (op, params) = exec regs op params

run :: Registers -> Program -> Registers
run initial program = snd $ fromJust $ last $ takeWhile isJust $ iterate (step program) (Just (0, initial))

step :: Program -> Maybe Memory -> Maybe Memory
step _ Nothing = Nothing
step (ipReg, instructions) (Just (currentIp, registers)) = if outOfBounds then Nothing else execute
  where execute = Just (newIp, newRegisters)
        newRegisters = execInstruction registersWithIp currentInstruction
        registersWithIp = replace ipReg currentIp registers
        newIp = (newRegisters !! ipReg) + 1
        outOfBounds = currentIp >= length instructions || currentIp < 0
        currentInstruction = instructions !! currentIp

day19a :: Program :~> Int
day19a = MkSol
    { sParse = parseMaybe sourceCode
    , sShow  = show
    , sSolve = Just . (!!0) . run [0,0,0,0,0,0]
    }

day19b :: Program :~> Int
day19b = MkSol
    { sParse = parseMaybe sourceCode
    , sShow  = show
    , sSolve = Just . (!!0) . run [1,0,0,0,0,0]
    }
