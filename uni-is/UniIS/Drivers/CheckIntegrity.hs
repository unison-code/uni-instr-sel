{-|
Copyright   :  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
License     :  BSD3 (see the LICENSE file)
Maintainer  :  ghb@kth.se
-}
{-
Main authors:
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module UniIS.Drivers.CheckIntegrity
  ( run )
where

import UniIS.Drivers.Base
import Language.InstrSel.Constraints
import Language.InstrSel.Constraints.ConstraintFolder
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.OpTypes
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )
import Language.InstrSel.Utils
  ( group )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit )

import Data.List
  ( nubBy
  , sort
  )

import Data.Maybe
  ( isJust
  , fromJust
  )



--------------
-- Data types
--------------

data CheckType
  = FunctionCheck
  | PatternCheck
  deriving (Eq)



-------------
-- Functions
-------------

run
  :: CheckAction
  -> Either Function InstrPattern
  -> IO [Output]

run CheckFunctionIntegrity (Left fun) =
  do let os = functionOS fun
         g = osGraph os
         log0 = checkGraphInvariants FunctionCheck g
         log1 = concatLogs $
                map ( \nid ->
                      checkNodeExists ( "function input with node ID " ++
                                        pShow nid
                                      )
                                      g
                                      nid
                    )
                    (functionInputs fun)
         log2 = checkEntryBlock os
         log3 = checkValueLocations os
         log4 = checkConstraints os
     return $ mkOutputFromLog $ concatLogs [log0, log1, log2, log3, log4]

run CheckPatternIntegrity (Right pat) =
  do let os = patOS pat
         g = osGraph os
         log0 = checkGraphInvariants PatternCheck g
         log1 = concatLogs $
                map ( \nid ->
                      checkNodeExists ("input data with ID " ++ pShow nid)
                                      g
                                      nid
                    )
                    (patInputData pat)
         log2 = concatLogs $
                map ( \nid ->
                      checkNodeExists ("output data with ID " ++ pShow nid)
                                      g
                                      nid
                    )
                    (patOutputData pat)
         log3 = checkEntryBlock os
         log4 = checkValueLocations os
         log5 = checkConstraints os
     return $ mkOutputFromLog $ concatLogs [log0, log1, log2, log3, log4, log5]

run _ _ = reportErrorAndExit "CheckIntegrity: unsupported action"

checkNodeExists :: String -> Graph -> NodeID -> Log
checkNodeExists msg g nid =
  let ns = findNodesWithNodeID g nid
  in if length ns == 0
     then Log [ ErrorMessage $ "Non-existing node: " ++ msg ]
     else emptyLog

checkGraphInvariants :: CheckType -> Graph -> Log
checkGraphInvariants c g =
  let check n =
        let log0 = nodeCheck n
            in_edges = getInEdges g n
            log1 = concatLogs $
                   map edgeNodeCheck in_edges
            log2 = inEdgeOrderCheck n in_edges
            out_edges = getOutEdges g n
            log3 = concatLogs $
                   map edgeNodeCheck out_edges
            log4 = outEdgeOrderCheck n out_edges
        in concatLogs [log0, log1, log2, log3, log4]
      nodeCheck n =
        case (getNodeType n) of
          (ComputationNode op) ->
            let log0 = checkNumInDtFlowEdges  g n (numOperands op)
                log1 = checkNumOutDtFlowEdges g n ( if producesValue op
                                                    then 1 else 0 )
                log2 = checkNumInStFlowEdges  g n ( if requiresState op
                                                    then 1 else 0 )
                log3 = checkNumOutStFlowEdges g n ( if requiresState op
                                                    then 1 else 0 )
                log4 = checkNumInCtrlFlowEdges  g n 0
                log5 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3, log4, log5]
          (ControlNode op) ->
            let log0 = if op /= Ret
                       then checkNumInDtFlowEdges  g n (numOperands op)
                       else emptyLog
                log1 = checkNumOutDtFlowEdges g n ( if producesValue op
                                                    then 1 else 0 )
                log2 = checkNumInStFlowEdges  g n ( if requiresState op
                                                    then 1 else 0 )
                log3 = checkNumOutStFlowEdges g n ( if requiresState op
                                                    then 1 else 0 )
                log4 = checkNumInCtrlFlowEdges  g n (numInCtrlFlows op)
                log5 = checkNumOutCtrlFlowEdges g n (numOutCtrlFlows op)
            in concatLogs [log0, log1, log2, log3, log4, log5]
          (CallNode {}) ->
            let log0 = checkNumInStFlowEdges  g n 1
                log1 = checkNumOutStFlowEdges g n 1
                log2 = checkNumInCtrlFlowEdges  g n 0
                log3 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3]
          IndirCallNode ->
            let log0 = checkNumInStFlowEdges  g n 1
                log1 = checkNumOutStFlowEdges g n 1
                log2 = checkNumInCtrlFlowEdges  g n 0
                log3 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3]
          (ValueNode {}) ->
            let log0 = if c == FunctionCheck
                       then checkNumInDtFlowEdges g n 1
                       else emptyLog
                log1 = if c == FunctionCheck
                       then checkNumOutDtFlowEdgesAtLeast g n 1 True
                       else emptyLog
                log2 = checkNumInStFlowEdges  g n 0
                log3 = checkNumOutStFlowEdges g n 0
                log4 = checkNumInCtrlFlowEdges  g n 0
                log5 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3, log4, log5]
          (BlockNode {}) ->
            let log0 = checkNumInDtFlowEdges  g n 0
                log1 = checkNumInStFlowEdges  g n 0
            in concatLogs [log0, log1]
          PhiNode ->
            let log0 = checkNumInDtFlowEdgesAtLeast g n 1
                log1 = checkNumOutDtFlowEdges g n 1
                log2 = checkNumInStFlowEdges  g n 0
                log3 = checkNumOutStFlowEdges g n 0
                log4 = checkNumInCtrlFlowEdges  g n 0
                log5 = checkNumOutCtrlFlowEdges g n 0
                log6 = checkPhiDefEdges g n
            in concatLogs [log0, log1, log2, log3, log4, log5, log6]
          StateNode ->
            let log0 = if c == FunctionCheck
                       then checkNumInStFlowEdges  g n 1
                       else emptyLog
                log1 = if c == FunctionCheck
                       then checkHasOutStFlowEdgeOrInDefEdge g n
                       else emptyLog
                log2 = checkNumInDtFlowEdges  g n 0
                log3 = checkNumOutDtFlowEdges g n 0
                log4 = checkNumInCtrlFlowEdges  g n 0
                log5 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3, log4, log5]
          CopyNode ->
            let log0 = checkNumInDtFlowEdges  g n 1
                log1 = checkNumOutDtFlowEdges g n 1
                log2 = checkNumInStFlowEdges  g n 0
                log3 = checkNumOutStFlowEdges g n 0
                log4 = checkNumInCtrlFlowEdges  g n 0
                log5 = checkNumOutCtrlFlowEdges g n 0
            in concatLogs [log0, log1, log2, log3, log4, log5]
      edgeNodeCheck e =
        let src = getSourceNode g e
            trg = getTargetNode g e
            src_type = getNodeType src
            trg_type = getNodeType trg
        in case (getEdgeType e) of
            ControlFlowEdge ->
              if not (isBlockNode src || isControlNode src)
              then toLog $
                   ErrorMessage $
                   "Invalid source node type: " ++ pShow e ++ " has " ++
                   pShow src_type ++ ", expected either block or control node"
              else if not (isBlockNode trg || isControlNode trg)
                   then toLog $
                        ErrorMessage $
                        "Invalid target node type: " ++ pShow e ++ " has " ++
                        pShow trg_type ++ ", expected either block or " ++
                        "control node"
                   else if isBlockNode src && not (isControlNode trg)
                        then toLog $
                             ErrorMessage $
                             "Invalid target node type: " ++ pShow e ++
                             " has " ++ pShow trg_type ++ ", expected " ++
                             "control node"
                        else if isControlNode src && not (isBlockNode trg)
                             then toLog $
                                  ErrorMessage $
                                  "Invalid target node type: " ++ pShow e ++
                                  " has " ++ pShow trg_type ++ ", " ++
                                  "expected block node"
                             else emptyLog
            DataFlowEdge ->
              if not (isOperationNode src || isBlockNode src || isValueNode src)
              then toLog $
                   ErrorMessage $
                   "Invalid source node type: " ++ pShow e ++ " has " ++
                   pShow src_type ++ ", expected either a computation, " ++
                   "control, call, phi, copy, block, or value node"
              else if not (isOperationNode trg || isValueNode trg)
                   then toLog $
                        ErrorMessage $
                        "Invalid target node type: " ++ pShow e ++ " has " ++
                        pShow trg_type ++ ", expected either a " ++
                        "computation, control, call, phi, copy, or value node"
                   else if isOperationNode src && not (isValueNode trg)
                        then toLog $
                             ErrorMessage $
                             "Invalid target node type: " ++ pShow e ++
                             " has " ++ pShow trg_type ++ ", expected " ++
                             "value node"
                        else if isValueNode src && not (isOperationNode trg)
                             then toLog $
                                  ErrorMessage $
                                  "Invalid target node type: " ++ pShow e ++
                                  " has " ++ pShow trg_type ++ ", " ++
                                  "expected computation, control, call, " ++
                                  "phi, or copy node"
                             else if isBlockNode src && not (isValueNode trg)
                                  then toLog $
                                       ErrorMessage $
                                       "Invalid target node type: " ++
                                       pShow e ++ " has " ++ pShow trg_type ++
                                       ", expected value node"
                                  else emptyLog
            StateFlowEdge ->
              if not (isOperationNode src || isBlockNode src || isStateNode src)
              then toLog $
                   ErrorMessage $
                   "Invalid source node type: " ++ pShow e ++ " has " ++
                   pShow src_type ++ ", expected either a computation, " ++
                   "control, call, phi, copy, block or state node"
              else if not (isOperationNode trg || isStateNode trg)
                   then toLog $
                        ErrorMessage $
                        "Invalid target node type: " ++ pShow e ++ " has " ++
                        pShow trg_type ++ ", expected either a " ++
                        "computation, control, call, phi, copy, or state node"
                   else if isOperationNode src && not (isStateNode trg)
                        then toLog $
                             ErrorMessage $
                             "Invalid target node type: " ++ pShow e ++
                             " has " ++ pShow trg_type ++ ", expected " ++
                             "state node"
                        else if isStateNode src && not (isOperationNode trg)
                             then toLog $
                                  ErrorMessage $
                                  "Invalid target node type: " ++ pShow e ++
                                  " has " ++ pShow trg_type ++ ", " ++
                                  "expected computation, control, call, " ++
                                  "phi, or copy node"
                             else if isBlockNode src && not (isStateNode trg)
                                  then toLog $
                                       ErrorMessage $
                                       "Invalid target node type: " ++
                                       pShow e ++ " has " ++ pShow trg_type ++
                                       ", expected state node"
                                  else emptyLog
            DefEdge ->
              if not (isBlockNode src || isValueNode src || isStateNode src)
              then toLog $
                   ErrorMessage $
                   "Invalid source node type: " ++ pShow e ++ " has " ++
                   pShow src_type ++ ", expected either block, value or " ++
                   "state node"
              else if not ( isBlockNode src ||
                            isValueNode src ||
                            isStateNode src
                          )
                   then toLog $
                        ErrorMessage $
                        "Invalid target node type: " ++ pShow e ++ " has " ++
                        pShow trg_type ++ ", expected either block, value " ++
                        "or state node"
                   else if isBlockNode src &&
                           not (isValueNode trg || isStateNode trg)
                        then toLog $
                             ErrorMessage $
                             "Invalid target node type: " ++ pShow e ++
                             " has " ++ pShow trg_type ++ ", expected " ++
                             "value or state node"
                        else if isValueNode src && not (isBlockNode trg)
                             then toLog $
                                  ErrorMessage $
                                  "Invalid target node type: " ++ pShow e ++
                                  " has " ++ pShow trg_type ++ ", " ++
                                  "expected block node"
                             else if isStateNode src && not (isBlockNode trg)
                                  then toLog $
                                       ErrorMessage $
                                       "Invalid target node type: " ++
                                       pShow e ++ " has " ++ pShow trg_type ++
                                       ", expected block node"
                                  else emptyLog
      inEdgeOrderCheck n es =
        let dt_es = filter isDataFlowEdge es
            st_es = filter isStateFlowEdge es
            ctrl_es = filter isControlFlowEdge es
            def_es = filter isDefEdge es
            log0 = checkDuplicateNumbers n "inbound data-flow edges" $
                   map getEdgeInNr dt_es
            log1 = checkDuplicateNumbers n "inbound state-flow edges" $
                   map getEdgeInNr st_es
            log2 = checkDuplicateNumbers n "inbound control-flow edges" $
                   map getEdgeInNr ctrl_es
            log3 = checkDuplicateNumbers n "inbound definition edges" $
                   map getEdgeInNr def_es
        in if isOperationNode n
           then let log4 = checkNumberOrder n "inbound data-flow edges" $
                           map getEdgeInNr dt_es
                    log5 = checkNumberOrder n "inbound state-flow edges" $
                           map getEdgeInNr st_es
                    log6 = checkNumberOrder n "inbound control-flow edges" $
                           map getEdgeInNr ctrl_es
                in concatLogs [log0, log1, log2, log3, log4, log5, log6]
           else concatLogs [log0, log1, log2, log3]
      outEdgeOrderCheck n es =
        let dt_es = filter isDataFlowEdge es
            st_es = filter isStateFlowEdge es
            ctrl_es = filter isControlFlowEdge es
            def_es = filter isDefEdge es
            log0 = checkDuplicateNumbers n "outbound data-flow edges" $
                   map getEdgeOutNr dt_es
            log1 = checkDuplicateNumbers n "outbound state-flow edges" $
                   map getEdgeOutNr st_es
            log2 = checkDuplicateNumbers n "outbound control-flow edges" $
                   map getEdgeOutNr ctrl_es
            log3 = checkDuplicateNumbers n "outbound definition edges" $
                   map getEdgeOutNr def_es
        in if isOperationNode n
           then let log4 = checkNumberOrder n "outbound data-flow edges" $
                           map getEdgeOutNr dt_es
                    log5 = checkNumberOrder n "outbound state-flow edges" $
                           map getEdgeOutNr st_es
                    log6 = checkNumberOrder n "outbound control-flow edges" $
                           map getEdgeOutNr ctrl_es
                in concatLogs [log0, log1, log2, log3, log4, log5, log6]
           else if isBlockNode n
                then let log4 = checkNumberOrder n ( "outbound control-flow " ++
                                                     "edges"
                                                   ) $
                                map getEdgeOutNr ctrl_es
                     in concatLogs [log0, log1, log2, log3, log4]
                else concatLogs [log0, log1, log2, log3]
      checkDuplicateNumbers n e_type ns =
        let grouped = group ns
        in concatLogs $
           map ( \ns' ->
                 if length ns' > 1
                 then toLog $
                      ErrorMessage $
                      "Inconsistent edge order: " ++ pShow n ++
                      " has multiple " ++ e_type ++ " edges the same edge " ++
                      "number " ++ pShow (head ns') ++ ", expected at most " ++
                      "one edge"
                 else emptyLog
               ) $
           grouped
      checkNumberOrder _ _ [] = emptyLog
      checkNumberOrder n e_type ns =
        let sorted = sort $ map fromIntegral ns
        in if length sorted /= (last sorted + 1)
           then toLog $
                ErrorMessage $
                "Inconsistent edge order: " ++ pShow n ++ " has " ++ e_type ++
                " edges with order " ++ pShow sorted ++ ", expected " ++
                pShow (take (length sorted) [0..] :: [Int])
           else emptyLog
  in concatLogs $
     map check $
     getAllNodes g

numInCtrlFlows :: ControlOp -> Int
numInCtrlFlows Br = 1
numInCtrlFlows CondBr = 1
numInCtrlFlows Ret = 1

numOutCtrlFlows :: ControlOp -> Int
numOutCtrlFlows Br = 1
numOutCtrlFlows CondBr = 2
numOutCtrlFlows Ret = 0

checkNumInDtFlowEdges :: Graph -> Node -> Int -> Log
checkNumInDtFlowEdges g n exp_num =
  let act_num = length $
                nubBy haveSameInEdgeNrs $
                getDtFlowInEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of inbound data-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkNumInDtFlowEdgesAtLeast :: Graph -> Node -> Int -> Log
checkNumInDtFlowEdgesAtLeast g n exp_num =
  let act_num = length $
                nubBy haveSameInEdgeNrs $
                getDtFlowInEdges g n
  in if act_num < exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of inbound data-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected at least " ++ pShow exp_num
     else emptyLog

checkNumOutDtFlowEdges :: Graph -> Node -> Int -> Log
checkNumOutDtFlowEdges g n exp_num =
  let act_num = length $
                nubBy haveSameOutEdgeNrs $
                getDtFlowOutEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of outbound data-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkNumOutDtFlowEdgesAtLeast :: Graph -> Node -> Int -> Bool -> Log
checkNumOutDtFlowEdgesAtLeast g n exp_num warn =
  let act_num = length $
                nubBy haveSameOutEdgeNrs $
                getDtFlowOutEdges g n
  in if act_num < exp_num
     then let str = "Wrong number of outbound data-flow edges: " ++ pShow n ++
                    " has " ++ pShow act_num ++ ", expected at least " ++
                    pShow exp_num
          in if warn then Log [WarningMessage str] else Log [ErrorMessage str]
     else emptyLog

checkNumInCtrlFlowEdges :: Graph -> Node -> Int -> Log
checkNumInCtrlFlowEdges g n exp_num =
  let act_num = length $ getCtrlFlowInEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of inbound control-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkNumOutCtrlFlowEdges :: Graph -> Node -> Int -> Log
checkNumOutCtrlFlowEdges g n exp_num =
  let act_num = length $ getCtrlFlowOutEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of outbound control-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkNumInStFlowEdges :: Graph -> Node -> Int -> Log
checkNumInStFlowEdges g n exp_num =
  let act_num = length $ getStFlowInEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of inbound state-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkNumOutStFlowEdges :: Graph -> Node -> Int -> Log
checkNumOutStFlowEdges g n exp_num =
  let act_num = length $ getStFlowOutEdges g n
  in if act_num /= exp_num
     then toLog $
          ErrorMessage $
          "Wrong number of outbound state-flow edges: " ++ pShow n ++
          " has " ++ pShow act_num ++ ", expected " ++ pShow exp_num
     else emptyLog

checkHasOutStFlowEdgeOrInDefEdge :: Graph -> Node -> Log
checkHasOutStFlowEdgeOrInDefEdge g n =
  let num_st_es = length $ getStFlowOutEdges g n
      num_def_es = length $ getDefInEdges g n
  in if num_st_es /= 1 && num_def_es /= 1
     then toLog $
          ErrorMessage $
          "Wrong number of outbound state-flow or inbound definition " ++
          "edges: " ++
          pShow n ++ " has " ++ pShow num_st_es ++ " state-flow edges and " ++
          pShow num_def_es ++ " definition edges, expected either " ++
          "1 state-flow edge or 1 definition edge (but not both)"
     else emptyLog

checkEntryBlock :: OpStructure -> Log
checkEntryBlock os =
  let g = osGraph os
      entry_n = entryBlockNode g
  in if isJust entry_n
     then let nid = getNodeID $ fromJust entry_n
          in checkNodeExists ("entry block node with ID " ++ pShow nid) g nid
     else emptyLog

checkValueLocations :: OpStructure -> Log
checkValueLocations os =
  let g = osGraph os
  in concatLogs $
     map ( \(nid, _) ->
           checkNodeExists ( "valid location specification with ID " ++
                             pShow nid
                           )
                           g
                           nid
         )
         (osValidLocations os)

checkPhiDefEdges :: Graph -> Node -> Log
checkPhiDefEdges g n =
  let in_es = getDtFlowInEdges g n
      out_es = getDtFlowOutEdges g n
      checkInEdge e =
        let src = getSourceNode g e
            nr = getEdgeOutNr e
            num_def_es = length $
                         filter (\e' -> getEdgeOutNr e' == nr) $
                         getDefOutEdges g src
        in if num_def_es /= 1
           then toLog $
                ErrorMessage $
                "Wrong number of outbound definition edges: " ++ pShow src ++
                ", which is predecessor of " ++ pShow n ++ ", has " ++
                pShow num_def_es ++ " definition edges with out-edge-" ++
                "number " ++ pShow nr ++ ", expected 1"
           else emptyLog
      checkOutEdge e =
        let trg = getTargetNode g e
            num_def_es = length $ getDefInEdges g trg
        in if num_def_es /= 1
           then toLog $
                ErrorMessage $
                "Wrong number of inbound definition edges: " ++ pShow trg ++
                ", which is successor of " ++ pShow n ++ ", has " ++
                pShow num_def_es ++ " definition edges, expected 1"
           else emptyLog
      log0 = concatLogs $
             map checkInEdge in_es
      log1 = concatLogs $
             map checkOutEdge out_es
  in concatLogs [log0, log1]

checkConstraints :: OpStructure -> Log
checkConstraints os =
  let g = osGraph os
      cs = osConstraints os
  in concatLogs $
     map (checkNodeInConstraint g) cs

checkNodeInConstraint :: Graph -> Constraint -> Log
checkNodeInConstraint g c =
  let def_f = mkDefaultFolder emptyLog appendLog
      foldNodeExpr _ (ANodeIDExpr n) =
        checkNodeExists ( "constraint " ++ toLispExpr c ++ " with node ID " ++
                          pShow n
                        )
                        g
                        n
      foldNodeExpr f expr =
        (foldNodeExprF def_f) f expr
      new_f = def_f { foldNodeExprF = foldNodeExpr }
  in apply new_f c
