{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day21
-- License     : BSD3

module AOC.Challenge.Day21 (
    day21a
  , day21b
  , sourceCode
  , allValues
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

-- The sequence of values in register 4 when pc is 28
-- At this point the program compares reg 4 and 0 and exits if they are equal
-- So this is also "all values for which the program halts"
allValues :: Program -> [Int]
allValues program = map (\(pc, regs) -> regs !! 4) $ filter (\(pc, regs) -> pc == 28) $ map fromJust $ iterate (step program) (Just (0, initial))
  where initial = replicate 6 0

-- The sequence above is periodic, it will eventually repeat; if register 0
-- doesn't contain a value in the sequence the program will loop forever; so
-- the value that yields the most instructions executed is the last value seen
-- before the sequence starts to repeat
lastUnique :: Ord a => [a] -> Maybe a
lastUnique list = head $ reverse $ takeWhile isJust $ zipWith (\a b-> a <$ guard (not $ S.member a b)) list (scanl (flip S.insert) S.empty list)

step :: Program -> Maybe Memory -> Maybe Memory
step _ Nothing = Nothing
step (ipReg, instructions) (Just (currentIp, registers)) = if outOfBounds then Nothing else execute
  where execute = Just (newIp, newRegisters)
        newRegisters = execInstruction registersWithIp currentInstruction
        registersWithIp = replace ipReg currentIp registers
        newIp = (newRegisters !! ipReg) + 1
        outOfBounds = currentIp >= length instructions || currentIp < 0
        currentInstruction = instructions !! currentIp

day21a :: Program :~> Int
day21a = MkSol
    { sParse = parseMaybe sourceCode
    , sShow  = show
    , sSolve = Just . head . allValues
    }

day21b :: Program :~> Int
day21b = MkSol
    { sParse = parseMaybe sourceCode
    , sShow  = show
    , sSolve = Just . lastUnique . allValues
    }
