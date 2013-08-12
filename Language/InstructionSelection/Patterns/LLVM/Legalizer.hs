--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.LLVM.Legalizer
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Legalizes an instruction by performing a series of modifications such as
-- alias resolving.
--
--------------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}

module Language.InstructionSelection.Patterns.LLVM.Legalizer (
  resolveAliases
) where

import Language.InstructionSelection.Patterns.LLVM.Base
import Language.InstructionSelection.Utils (Range (..))



-- | Resolves aliases by replacing all occurrances of aliased temporaries by a
-- single temporary selected from the alias set. The alias constraint is
-- thereafter removed.

resolveAliases :: Instruction -> Instruction
resolveAliases (Instruction str ps) =
  Instruction str (map resolveAliases' ps)

resolveAliases' :: Pattern -> Pattern
resolveAliases' p =
  let aliases = getAliases p
  in foldr processAliasList p aliases

-- | Processes a list of alias values on a pattern and returns the corresponding
-- pattern where all occurrances of values in the code have been replaced by use
-- of the first alias value in the list.

processAliasList :: [AliasValue] -> Pattern -> Pattern
processAliasList avs p =
  foldr (replaceTemp $ selected_alias) p (filter (/= selected_alias) avs)
  where selected_alias = selectAliasValueToKeep avs

-- | Select an alias value to keep. If there are any registers, one of these
-- will be selected first. If there are no such values, a temporary will be
-- selected.

selectAliasValueToKeep :: [AliasValue] -> AliasValue
selectAliasValueToKeep avs =
  let registers = filter isAVRegister avs
  in if not $ null registers
     then head registers
     else head avs

class TempReplaceable a where

  -- | Pretty-prints something as an S-expression by converting it into a
  -- string.

  replaceTemp :: AliasValue     -- ^ Alias to replace with.
              -> AliasValue     -- ^ Alias to replace.
              -> a
              -> a

-- | Replaces the use of one temp for another.

instance TempReplaceable Pattern where
  replaceTemp d s (Pattern stmts cons) =
    Pattern (map (replaceTemp d s) stmts) (map (replaceTemp d s) pruned_cons)
    where pruned_cons = filter (not . isAliasesConstraint) cons

instance TempReplaceable Statement where
  replaceTemp d s (AssignmentStmt temp expr) =
    AssignmentStmt (replaceTemp d s temp) (replaceTemp d s expr)
  replaceTemp d s (SetRegStmt reg expr) =
    SetRegStmt (replaceTemp d s reg) (replaceTemp d s expr)
  replaceTemp d s (StoreStmt expr1 str size expr2) =
    StoreStmt (replaceTemp d s expr1) str size (replaceTemp d s expr2)
  replaceTemp d s (CondBranchStmt reg label1 label2) =
    CondBranchStmt (replaceTemp d s reg) label1 label2
  replaceTemp _ _ stmt = stmt

instance TempReplaceable Constraint where
  replaceTemp d s (AllocateInConstraint store space) =
    AllocateInConstraint (replaceTemp d s store) space
  replaceTemp _ _ c = c

instance TempReplaceable AnyStorage where
  replaceTemp d (AVTemporary s) store@(ASTemporary temp) =
    if s == temp
    then convertAliasValueToAnyStorage d
    else store
  replaceTemp d (AVRegister s) store@(ASRegister reg) =
    if s == reg
    then convertAliasValueToAnyStorage d
    else store
  replaceTemp _ _ store = store

instance TempReplaceable StmtExpression where
  replaceTemp d s (BinaryOpStmtExpr op size expr1 expr2) =
    BinaryOpStmtExpr op size (replaceTemp d s expr1) (replaceTemp d s expr2)
  replaceTemp d s (UnaryOpStmtExpr op size expr) =
    UnaryOpStmtExpr op size (replaceTemp d s expr)
  replaceTemp d s (LoadStmtExpr area size expr) =
    LoadStmtExpr area size (replaceTemp d s expr)
  replaceTemp d s (FP2IStmtExpr size1 expr size2) =
    FP2IStmtExpr size1 (replaceTemp d s expr) size2
  replaceTemp d s (TruncStmtExpr size1 expr size2) =
    TruncStmtExpr size1 (replaceTemp d s expr) size2
  replaceTemp d s (PhiStmtExpr phis) =
    PhiStmtExpr (map (replaceTemp d s) phis)
  replaceTemp d s (DataStmtExpr expr) = DataStmtExpr (replaceTemp d s expr)
  replaceTemp d s (SizeStmtExpr reg) = SizeStmtExpr (replaceTemp d s reg)
  replaceTemp d s (RegRangeStmtExpr reg range) =
    RegRangeStmtExpr (replaceTemp d s reg) (replaceTemp d s range)

instance TempReplaceable PhiElement where
  replaceTemp d s (PhiElement expr label) =
    PhiElement (replaceTemp d s expr) label

instance TempReplaceable (Range AnyData) where
  replaceTemp d s (Range lower upper) =
    Range (replaceTemp d s lower) (replaceTemp d s upper)

instance TempReplaceable Temporary where
  replaceTemp (AVTemporary d) (AVTemporary s) temp =
    if s == temp then d else temp
  replaceTemp _ _ temp = temp

instance TempReplaceable Register where
  replaceTemp (AVRegister d) (AVRegister s) reg =
    if s == reg then d else reg
  replaceTemp _ _ reg = reg

instance TempReplaceable ProgramData where
  replaceTemp d (AVTemporary s) store@(PDTemporary temp) =
    if s == temp
    then convertAliasValueToProgramData d
    else store
  replaceTemp d (AVRegister s) store@(PDRegister reg) =
    if s == reg
    then convertAliasValueToProgramData d
    else store
  replaceTemp _ _ pdata = pdata

instance TempReplaceable AnyData where
  replaceTemp d (AVTemporary s) store@(ADTemporary temp) =
    if s == temp
    then convertAliasValueToAnyData d
    else store
  replaceTemp d (AVRegister s) store@(ADRegister reg) =
    if s == reg
    then convertAliasValueToAnyData d
    else store
  replaceTemp _ _ pdata = pdata

convertAliasValueToAnyStorage (AVTemporary d) = ASTemporary d
convertAliasValueToAnyStorage (AVRegister d) = ASRegister d

convertAliasValueToProgramData (AVTemporary d) = PDTemporary d
convertAliasValueToProgramData (AVRegister d) = PDRegister d

convertAliasValueToAnyData (AVTemporary d) = ADTemporary d
convertAliasValueToAnyData (AVRegister d) = ADRegister d

-- | Gets the aliases from a pattern.

getAliases :: Pattern -> [[AliasValue]]
getAliases (Pattern _ cs) =
  let aliases_list = [as | (AliasesConstraint as) <- cs]
      aliases = foldr (++) [] aliases_list
      no_novalue_aliases = map removeNoValueAliases aliases
      no_empty_aliases = filter (not . null) no_novalue_aliases
  in no_empty_aliases

-- | Remove all alias values that signify 'no value'.

removeNoValueAliases :: [AliasValue] -> [AliasValue]
removeNoValueAliases avs = filter (not . isAVNoValue) avs
