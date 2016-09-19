{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Functions.LLVM.FunctionMaker
  ( mkFunctionsFromLlvmModule
  , mkFunction
  )
where

import qualified Language.InstrSel.Graphs as G
import qualified Language.InstrSel.OpStructures as OS
import Language.InstrSel.OpStructures.LLVM.OSMaker
import qualified Language.InstrSel.Functions as F

import qualified LLVM.General.AST as LLVM
import qualified LLVM.General.AST.Constant as LLVMC
import qualified LLVM.General.AST.Global as LLVM

import Data.Maybe



-------------
-- Functions
-------------

-- | Builds a list of functions from an LLVM module. If the module does not
-- contain any globally defined functions, an empty list is returned. If any of
-- the functions produce an error, only the first encountered error is returned.
mkFunctionsFromLlvmModule
  :: LLVM.Module
  -> Either String [F.Function]
     -- ^ An error message or the built functions.
mkFunctionsFromLlvmModule m =
  sequence $
  mapMaybe (mkFunctionFromGlobalDef m) (LLVM.moduleDefinitions m)

-- | Builds a function from an LLVM AST definition. If the definition is not a
-- 'GlobalDefinition', 'Nothing' is returned.
mkFunctionFromGlobalDef
  :: LLVM.Module
  -> LLVM.Definition
  -> Maybe (Either String F.Function)
mkFunctionFromGlobalDef m (LLVM.GlobalDefinition g) = mkFunction m g
mkFunctionFromGlobalDef _ _ = Nothing

-- | Builds a function from a global LLVM AST definition.
mkFunction
  :: LLVM.Module
  -> LLVM.Global
  -> Maybe (Either String F.Function)
     -- ^ An error message or the built function.
mkFunction m f@(LLVM.Function {}) =
  if length (LLVM.basicBlocks f) > 0
     then Just $ do os <- mkFunctionOS f
                    let (params, _) = LLVM.parameters f
                    input_nodes <- mapM (extractFunctionInputNodeID os) params
                    let exec_freqs = extractBBExecFreqs m f
                    return F.Function
                             { F.functionName = toFunctionName $ LLVM.name f
                             , F.functionOS = os
                             , F.functionInputs = input_nodes
                             , F.functionBBExecFreq = exec_freqs
                             , F.functionBEBlocks = []
                             }
     else Nothing
mkFunction _ _ = Nothing

toFunctionName :: LLVM.Name -> Maybe String
toFunctionName (LLVM.Name str) = Just str
toFunctionName (LLVM.UnName _) = Nothing

-- | Extracts the ID of the node in the 'OpStructure' that corresponds to the
-- given function parameter.
extractFunctionInputNodeID
  :: OS.OpStructure
  -> LLVM.Parameter
  -> Either String G.NodeID
     -- An error message or the extracted node ID.
extractFunctionInputNodeID os (LLVM.Parameter _ name _) =
  do sym <- toSymbolString name
     let g = OS.osGraph os
         ns = G.findValueNodesWithOrigin g sym
     if length ns == 1
     then return $ G.getNodeID $ head ns
     else if length ns == 0
          then Left $ "extractFunctionInputNodeID: no value node with origin '"
                       ++ sym ++ "'"
          else Left $ "extractFunctionInputNodeID: more than one value node "
                       ++ "with origin '" ++ sym ++ "'"

-- | Extracts all basic blocks in the given function together with their
-- execution frequencies.
extractBBExecFreqs
  :: LLVM.Module
  -> LLVM.Global
  -> [(F.BlockName, F.ExecFreq)]
extractBBExecFreqs m f@(LLVM.Function {}) =
  map processBB (LLVM.basicBlocks f)
  where processBB (LLVM.BasicBlock name _ term_inst) =
          ( F.BlockName $ nameToString name
          , extractExecFreq m (LLVM.metadata' $ fromNamed term_inst)
          )
        fromNamed (_ LLVM.:= i) = i
        fromNamed (LLVM.Do i) = i
        nameToString (LLVM.Name str) = str
        nameToString (LLVM.UnName int) = show int
extractBBExecFreqs _ _ = error "extractBBExecFreqs: not a Function"

-- | Extracts the block execution frequency from the metadata (which should be
-- attached to the terminator instruction of the corresponding basic block).
extractExecFreq :: LLVM.Module -> LLVM.InstructionMetadata -> F.ExecFreq
extractExecFreq m im =
  mkExecFreq $ head $ checkNumOps $ getOps $ head $ checkNumNodes $ findNodes im
  where soughtMetaName = "exec_freq"
        findNodes = map snd . filter (\m' -> fst m' == soughtMetaName)
        checkNumNodes ms | length ms == 0 =
                             error $
                             "No metadata entry with name '" ++
                             soughtMetaName ++ "'!"
                         | length ms > 1 =
                             error $
                             "Multiple metadata entries with name '" ++
                             soughtMetaName ++ "'!"
                         | otherwise = ms

        getOps = catMaybes . (retrieveMetadataOps m)
        checkNumOps ops | length ops == 0 = error "No operands in metadata!"
                        | length ops > 1 =
                            error "Multiple operands in metadata!"
                        | otherwise = ops
        mkExecFreq (LLVM.ConstantOperand (LLVMC.Int _ freq)) =
          F.toExecFreq freq
        mkExecFreq _ = error "Invalid execution frequency value!"

-- | Retrieves the list of operands attached to a metadata node. If the node is
-- a metanode ID, then the operands of that metanode ID will be retrieved.
retrieveMetadataOps :: LLVM.Module -> LLVM.MetadataNode -> [Maybe LLVM.Operand]
retrieveMetadataOps _ (LLVM.MetadataNode ops) =
  map (maybe Nothing f) ops
  where f (LLVM.MDValue o) = Just o
        f o = error $ "Unexpected metadata: " ++ show o
retrieveMetadataOps m (LLVM.MetadataNodeReference mid) =
  let module_defs = LLVM.moduleDefinitions m
      isMetaDef (LLVM.MetadataNodeDefinition _ _) = True
      isMetaDef _ = False
      meta_defs = filter isMetaDef module_defs
      sought_ops = mapMaybe ( \(LLVM.MetadataNodeDefinition mid' ops) ->
                              if mid' == mid then Just ops else Nothing
                            ) $
                   meta_defs
  in if length sought_ops == 1
     then let f (LLVM.MDValue o) = Just o
              f o = error $ "Unexpected metadata: " ++ show o
          in map (maybe Nothing f) $ head sought_ops
     else let (LLVM.MetadataNodeID mid_value) = mid
          in error $ "No metadata with ID " ++ (show mid_value)
