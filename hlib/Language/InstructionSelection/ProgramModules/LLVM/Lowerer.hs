--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.ProgramModules.LLVM.PMMaker
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Lowers a module AST by converting high-level instructions such as 'switch'
-- and 'getelementptr' into multiple lower-level LLVM IR instructions.
--------------------------------------------------------------------------------

module Language.InstructionSelection.ProgramModules.LLVM.Lowerer (
  lower
) where

import Data.List
import LLVM.General.AST
import LLVM.General.AST.Constant (Constant)
import qualified LLVM.General.AST.IntegerPredicate as IP



lower :: Module -> Module
lower m =
  Module (moduleName m)
         (moduleDataLayout m)
         (moduleTargetTriple m)
         (map lowerFunction $ moduleDefinitions m)

lowerFunction :: Definition -> Definition
lowerFunction (GlobalDefinition g) =
  GlobalDefinition $ lowerGlobalDef g
lowerFunction f = f

lowerGlobalDef :: Global -> Global
lowerGlobalDef (Function l v cc ps t n tup fs ms1 w ms2 bbs) =
  let acc = (map getBBLabel bbs, concatMap getAllTempNames bbs)
      (_, bbs_list) = mapAccumL lowerBB acc bbs
      new_bbs  = concat bbs_list
  in Function l v cc ps t n tup fs ms1 w ms2 new_bbs

getBBLabel :: BasicBlock -> Name
getBBLabel (BasicBlock l _ _) = l

getAllTempNames :: BasicBlock -> [Name]
getAllTempNames _ = []

lowerBB :: ([Name], [Name]) -> BasicBlock -> (([Name], [Name]), [BasicBlock])
lowerBB acc (BasicBlock label insts
                        (Do (Switch operand def_label dests _))) =
  let (new_acc, sw_bbs) = mapAccumL (mkBBFromCase operand def_label) acc dests
      (BasicBlock _ cmp_insts sw_term) = head sw_bbs
  in (new_acc, (BasicBlock label (insts ++ cmp_insts) sw_term):(tail sw_bbs))
lowerBB acc bb = (acc, [bb])

mkBBFromCase :: Operand
             -> Name
             -> ([Name], [Name])
             -> (Constant, Name)
             -> (([Name], [Name]), BasicBlock)
mkBBFromCase op_value f_label (all_labels, all_tmp_names) (cmp_val, label) =
  let cmp_result_name = getNewTempName all_tmp_names
      cmp_inst = cmp_result_name := ICmp IP.EQ op_value
                                         (ConstantOperand cmp_val) []
      new_label = getNewBBLabel all_labels
      br_inst = Do $ CondBr (LocalReference cmp_result_name) label f_label []
      new_acc = (new_label:all_labels, cmp_result_name:all_tmp_names)
  in (new_acc, BasicBlock new_label [cmp_inst] br_inst)

getNewBBLabel :: [Name] -> Name
getNewBBLabel names = getNewBBLabel' 0 names
getNewBBLabel' :: Int -> [Name] -> Name
getNewBBLabel' i names =
  let label = Name $ "sw" ++ show i
  in if not $ label `elem` names
     then label
     else getNewBBLabel' (i + 1) names

getNewTempName :: [Name] -> Name
getNewTempName names = getNewTempName' 0 names
getNewTempName' :: Int -> [Name] -> Name
getNewTempName' i names =
  let name = Name $ "cmp" ++ show i
  in if not $ name `elem` names
     then name
     else getNewTempName' (i + 1) names
