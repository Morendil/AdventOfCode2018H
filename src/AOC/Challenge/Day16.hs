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
import Data.List.Extra (nubOn)
import Data.List.HT (takeUntil)
import qualified Data.Map as M
import qualified Data.Set as S

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

puzzle = do
  samples <- sepBy1 sample (string "\n\n")
  string "\n\n\n\n"
  program <- sepBy1 instruction (string "\n")
  return (samples, program)

type Registers = [Int]
type Instruction = [Int]
type Parameters = [Int]
type Program = [Instruction]
type Samples = [Sample]
type Sample = (Instruction, (Registers, Registers))
type Puzzle = (Samples, Program)

data Mode = Register | Value | Ignore
type OpCode = (String, Mode, Mode, Int -> Int -> Int)

type Solution = M.Map Int OpCode

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

execInstruction :: Solution -> Registers -> Instruction -> Registers
execInstruction map regs (num:params) = exec regs op params
  where op = fromJust $ M.lookup num map

run :: Solution -> [Instruction] -> Registers
run map program = foldl (execInstruction map) [0,0,0,0] program

behavesLike :: Sample -> OpCode -> Bool
behavesLike (instruction, (before, after)) op = after == exec before op (tail instruction)

matching :: Sample -> [OpCode] -> [OpCode]
matching sample = filter (behavesLike sample)

complete :: Solution -> Bool
complete sol = M.keysSet sol == opNums

opNums :: S.Set Int
opNums = S.fromList [0..15]

name :: OpCode -> String
name (_name, _, _, _) = _name

investigate :: [OpCode] -> Sample -> Maybe (Int, OpCode)
investigate unmapped sample = if length candidates > 1 || null candidates then Nothing else Just result
  where candidates = matching sample unmapped
        result = (opNum, opCode)
        ((opNum:_), _) = sample
        opCode = head candidates

deduce :: [Sample] -> ([OpCode], Solution) -> ([OpCode], Solution)
deduce samples (unmapped, partial) = (remaining, assigned)
  where unambiguous = mapMaybe (investigate unmapped) samples
        mapped = map snd $ nubOn fst unambiguous
        mappedNames = map name mapped
        remaining = filter (\op -> not $ name op `elem` mappedNames) unmapped
        assigned = foldr (uncurry M.insert) partial unambiguous

solve :: [Sample] -> Solution
solve samples = last $ takeUntil complete $ map snd $ iterate (deduce samples) (opcodes, M.empty)

solveAndExecute :: Puzzle -> Int
solveAndExecute (samples, program) = (run solution program) !! 0
  where solution = solve samples

day16a :: [Sample] :~> Int
day16a = MkSol
    { sParse = parseMaybe $ sepBy1 sample (string "\n\n")
    , sShow  = show
    , sSolve = Just . length . filter (\sample -> length (matching sample opcodes) >= 3)
    }

day16b :: Puzzle :~> Int
day16b = MkSol
    { sParse = parseMaybe puzzle
    , sShow  = show
    , sSolve = Just . solveAndExecute
    }
