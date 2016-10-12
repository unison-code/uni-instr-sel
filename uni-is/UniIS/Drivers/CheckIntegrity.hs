{-|
Copyright   :  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
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
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.OpTypes
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , errorExitCode
  )

import Data.List
  ( intercalate
  , nubBy
  , sort
  )

import Data.Maybe
  ( isJust
  , fromJust
  )



--------------
-- Data types
--------------

type ErrorMessage = String

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
         msg0 = checkGraphInvariants FunctionCheck g
         msg1 = concatMap ( \nid ->
                            checkNodeExists ( "function input with node ID " ++
                                              pShow nid
                                            )
                                            g
                                            nid
                          )
                          (functionInputs fun)
         msg2 = checkEntryBlock os
         msg3 = checkValueLocations os
         msg4 = checkConstraints os
     return $ mkOutput $ concat [msg0, msg1, msg2, msg3, msg4]

run CheckPatternIntegrity (Right pat) =
  do let os = patOS pat
         g = osGraph os
         msg0 = checkGraphInvariants PatternCheck g
         msg1 = concatMap ( \nid ->
                            checkNodeExists ( "external data with ID " ++
                                              pShow nid
                                            )
                                            g
                                            nid
                          )
                          (patExternalData pat)
         msg2 = checkEntryBlock os
         msg3 = checkValueLocations os
         msg4 = checkConstraints os
     return $ mkOutput $ concat [msg0, msg1, msg2, msg3, msg4]

run _ _ = reportErrorAndExit "CheckIntegrity: unsupported action"

-- | Makes an 'Output' from a given output log. An empty log indicates no
-- errors.
mkOutput :: [ErrorMessage] -> [Output]
mkOutput [] = [toOutput ""]
mkOutput msgs =
  [toOutputWithExitCode errorExitCode (intercalate "\n\n" msgs)]

checkNodeExists :: ErrorMessage -> Graph -> NodeID -> [ErrorMessage]
checkNodeExists msg g nid =
  let ns = findNodesWithNodeID g nid
  in if length ns == 0
     then [ "Non-existing node: " ++ msg ]
     else []

checkGraphInvariants :: CheckType -> Graph -> [ErrorMessage]
checkGraphInvariants c g =
  let check n =
        let msg0 = nodeCheck n
            in_edges = getInEdges g n
            msg1 = concatMap edgeNodeCheck in_edges
            msg2 = inEdgeOrderCheck n in_edges
            out_edges = getOutEdges g n
            msg3 = concatMap edgeNodeCheck out_edges
            msg4 = outEdgeOrderCheck n out_edges
        in concat [msg0, msg1, msg2, msg3, msg4]
      nodeCheck n =
        case (getNodeType n) of
          (ComputationNode op) ->
            let msg0 = checkNumInDtFlowEdges  g n (numOperands op)
                msg1 = checkNumOutDtFlowEdges g n ( if producesValue op
                                                    then 1 else 0 )
                msg2 = checkNumInStFlowEdges  g n ( if requiresState op
                                                    then 1 else 0 )
                msg3 = checkNumOutStFlowEdges g n ( if requiresState op
                                                    then 1 else 0 )
                msg4 = checkNumInCtrlFlowEdges  g n 0
                msg5 = checkNumOutCtrlFlowEdges g n 0
            in concat [msg0, msg1, msg2, msg3, msg4, msg5]
          (ControlNode op) ->
            let msg0 = if op /= Ret
                       then checkNumInDtFlowEdges  g n (numOperands op)
                       else []
                msg1 = checkNumOutDtFlowEdges g n ( if producesValue op
                                                    then 1 else 0 )
                msg2 = checkNumInStFlowEdges  g n ( if requiresState op
                                                    then 1 else 0 )
                msg3 = checkNumOutStFlowEdges g n ( if requiresState op
                                                    then 1 else 0 )
                msg4 = checkNumInCtrlFlowEdges  g n (numInCtrlFlows op)
                msg5 = checkNumOutCtrlFlowEdges g n (numOutCtrlFlows op)
            in concat [msg0, msg1, msg2, msg3, msg4, msg5]
          (CallNode {}) ->
            let msg0 = checkNumInStFlowEdges  g n 1
                msg1 = checkNumOutStFlowEdges g n 1
                msg2 = checkNumInCtrlFlowEdges  g n 0
                msg3 = checkNumOutCtrlFlowEdges g n 0
            in concat [msg0, msg1, msg2, msg3]
          (ValueNode {}) ->
            let msg0 = if c == FunctionCheck
                       then checkNumInDtFlowEdges g n 1
                       else []
                msg1 = if c == FunctionCheck
                       then checkNumOutDtFlowEdgesAtLeast g n 1
                       else []
                msg2 = checkNumInStFlowEdges  g n 0
                msg3 = checkNumOutStFlowEdges g n 0
                msg4 = checkNumInCtrlFlowEdges  g n 0
                msg5 = checkNumOutCtrlFlowEdges g n 0
            in concat [msg0, msg1, msg2, msg3, msg4, msg5]
          (BlockNode {}) ->
            let msg0 = checkNumInDtFlowEdges  g n 0
                msg1 = checkNumInStFlowEdges  g n 0
            in concat [msg0, msg1]
          PhiNode ->
            let msg0 = checkNumInDtFlowEdgesAtLeast g n 1
                msg1 = checkNumOutDtFlowEdges g n 1
                msg2 = checkNumInStFlowEdges  g n 0
                msg3 = checkNumOutStFlowEdges g n 0
                msg4 = checkNumInCtrlFlowEdges  g n 0
                msg5 = checkNumOutCtrlFlowEdges g n 0
                msg6 = checkPhiDefEdges g n
            in concat [msg0, msg1, msg2, msg3, msg4, msg5, msg6]
          StateNode ->
            let msg0 = if c == FunctionCheck
                       then checkNumInStFlowEdges  g n 1
                       else []
                msg1 = if c == FunctionCheck
                       then checkHasOutStFlowEdgeOrInDefEdge g n
                       else []
                msg2 = checkNumInDtFlowEdges  g n 0
                msg3 = checkNumOutDtFlowEdges g n 0
                msg4 = checkNumInCtrlFlowEdges  g n 0
                msg5 = checkNumOutCtrlFlowEdges g n 0
            in concat [msg0, msg1, msg2, msg3, msg4, msg5]
          CopyNode ->
            let msg0 = checkNumInDtFlowEdges  g n 1
                msg1 = checkNumOutDtFlowEdges g n 1
                msg2 = checkNumInStFlowEdges  g n 0
                msg3 = checkNumOutStFlowEdges g n 0
                msg4 = checkNumInCtrlFlowEdges  g n 0
                msg5 = checkNumOutCtrlFlowEdges g n 0
            in concat [msg0, msg1, msg2, msg3, msg4, msg5]
      edgeNodeCheck e =
        let src = getSourceNode g e
            trg = getTargetNode g e
            src_type = getNodeType src
            trg_type = getNodeType trg
        in case (getEdgeType e) of
            ControlFlowEdge ->
              if not (isBlockNode src || isControlNode src)
              then [ "Invalid source node type: " ++ show e ++ " has " ++
                     pShow src_type ++ ", expected either block or control node"
                   ]
              else if not (isBlockNode trg || isControlNode trg)
                   then [ "Invalid target node type: " ++ show e ++ " has " ++
                          pShow trg_type ++ ", expected either block or " ++
                          "control node"
                        ]
                   else if isBlockNode src && not (isControlNode trg)
                        then [ "Invalid target node type: " ++ show e ++
                               " has " ++ pShow trg_type ++ ", expected " ++
                               "control node"
                        ]
                        else if isControlNode src && not (isBlockNode trg)
                             then [ "Invalid target node type: " ++ show e ++
                                    " has " ++ pShow trg_type ++ ", " ++
                                    "expected block node"
                                  ]
                             else []
            DataFlowEdge ->
              if not (isOperationNode src || isBlockNode src || isValueNode src)
              then [ "Invalid source node type: " ++ show e ++ " has " ++
                     pShow src_type ++ ", expected either a computation, " ++
                     "control, call, phi, copy, block, or value node"
                   ]
              else if not (isOperationNode trg || isValueNode trg)
                   then [ "Invalid target node type: " ++ show e ++ " has " ++
                          pShow trg_type ++ ", expected either a " ++
                          "computation, control, call, phi, copy, or value node"
                        ]
                   else if isOperationNode src && not (isValueNode trg)
                        then [ "Invalid target node type: " ++ show e ++
                               " has " ++ pShow trg_type ++ ", expected " ++
                               "value node"
                        ]
                        else if isValueNode src && not (isOperationNode trg)
                             then [ "Invalid target node type: " ++ show e ++
                                    " has " ++ pShow trg_type ++ ", " ++
                                    "expected computation, control, call, " ++
                                    "phi, or copy node"
                                  ]
                             else if isBlockNode src && not (isValueNode trg)
                                  then [ "Invalid target node type: " ++
                                         show e ++ " has " ++ pShow trg_type ++
                                         ", expected value node"
                                       ]
                                  else []
            StateFlowEdge ->
              if not (isOperationNode src || isBlockNode src || isStateNode src)
              then [ "Invalid source node type: " ++ show e ++ " has " ++
                     pShow src_type ++ ", expected either a computation, " ++
                     "control, call, phi, copy, block or state node"
                   ]
              else if not (isOperationNode trg || isStateNode trg)
                   then [ "Invalid target node type: " ++ show e ++ " has " ++
                          pShow trg_type ++ ", expected either a " ++
                          "computation, control, call, phi, copy, or state node"
                        ]
                   else if isOperationNode src && not (isStateNode trg)
                        then [ "Invalid target node type: " ++ show e ++
                               " has " ++ pShow trg_type ++ ", expected " ++
                               "state node"
                        ]
                        else if isStateNode src && not (isOperationNode trg)
                             then [ "Invalid target node type: " ++ show e ++
                                    " has " ++ pShow trg_type ++ ", " ++
                                    "expected computation, control, call, " ++
                                    "phi, or copy node"
                                  ]
                             else if isBlockNode src && not (isStateNode trg)
                                  then [ "Invalid target node type: " ++
                                         show e ++ " has " ++ pShow trg_type ++
                                         ", expected state node"
                                       ]
                                  else []
            DefEdge ->
              if not (isBlockNode src || isValueNode src || isStateNode src)
              then [ "Invalid source node type: " ++ show e ++ " has " ++
                     pShow src_type ++ ", expected either block, value or " ++
                     "state node"
                   ]
              else if not ( isBlockNode src ||
                            isValueNode src ||
                            isStateNode src
                          )
                   then [ "Invalid target node type: " ++ show e ++ " has " ++
                          pShow trg_type ++ ", expected either block, value " ++
                          "or state node"
                        ]
                   else if isBlockNode src &&
                           not (isValueNode trg || isStateNode trg)
                        then [ "Invalid target node type: " ++ show e ++
                               " has " ++ pShow trg_type ++ ", expected " ++
                               "value or state node"
                        ]
                        else if isValueNode src && not (isBlockNode trg)
                             then [ "Invalid target node type: " ++ show e ++
                                    " has " ++ pShow trg_type ++ ", " ++
                                    "expected block node"
                                  ]
                             else if isStateNode src && not (isBlockNode trg)
                                  then [ "Invalid target node type: " ++
                                         show e ++ " has " ++ pShow trg_type ++
                                         ", expected block node"
                                       ]
                                  else []
      inEdgeOrderCheck n es =
        let dt_es = nubBy haveSameInEdgeNrs $
                    filter isDataFlowEdge $
                    es
            st_es = filter isStateFlowEdge es
            ctrl_es = filter isControlFlowEdge es
        in if isOperationNode n
           then let msg0 = checkNumberOrder n "inbound data-flow edges" $
                           map getInEdgeNr dt_es
                    msg1 = checkNumberOrder n "inbound state-flow edges" $
                           map getInEdgeNr st_es
                    msg2 = checkNumberOrder n "inbound control-flow edges" $
                           map getInEdgeNr ctrl_es
                in concat [msg0, msg1, msg2]
           else []
      outEdgeOrderCheck n es =
        let dt_es = filter isDataFlowEdge es
            st_es = filter isStateFlowEdge es
            ctrl_es = filter isControlFlowEdge es
        in if isOperationNode n
           then let msg0 = checkNumberOrder n "outbound data-flow edges" $
                           map getOutEdgeNr dt_es
                    msg1 = checkNumberOrder n "outbound state-flow edges" $
                           map getOutEdgeNr st_es
                    msg2 = checkNumberOrder n "outbound control-flow edges" $
                           map getOutEdgeNr ctrl_es
                in concat [msg0, msg1, msg2]
           else if isBlockNode n
                then checkNumberOrder n "outbound control-flow edges" $
                     map getOutEdgeNr ctrl_es
                else []
      checkNumberOrder _ _ [] = []
      checkNumberOrder n e_type ns =
        let sorted = sort $ map fromIntegral ns
        in if length sorted /= (last sorted + 1)
           then [ "Inconsistent edge order: " ++ show n ++ " has " ++ e_type ++
                  " edges with order " ++ pShow sorted ++ ", expected " ++
                  pShow (take (length sorted) [0..] :: [Int])
                ]
           else []
  in concatMap check $
     getAllNodes g

numInCtrlFlows :: ControlOp -> Int
numInCtrlFlows Br = 1
numInCtrlFlows CondBr = 1
numInCtrlFlows Ret = 1

numOutCtrlFlows :: ControlOp -> Int
numOutCtrlFlows Br = 1
numOutCtrlFlows CondBr = 2
numOutCtrlFlows Ret = 0

checkNumInDtFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumInDtFlowEdges g n exp_num =
  let act_num = length $
                nubBy haveSameInEdgeNrs $
                getDtFlowInEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of inbound data-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkNumInDtFlowEdgesAtLeast :: Graph -> Node -> Int -> [ErrorMessage]
checkNumInDtFlowEdgesAtLeast g n exp_num =
  let act_num = length $
                nubBy haveSameInEdgeNrs $
                getDtFlowInEdges g n
  in if act_num < exp_num
     then [ "Wrong number of inbound data-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected at least " ++ show exp_num
          ]
     else []

checkNumOutDtFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumOutDtFlowEdges g n exp_num =
  let act_num = length $
                nubBy haveSameOutEdgeNrs $
                getDtFlowOutEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of outbound data-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkNumOutDtFlowEdgesAtLeast :: Graph -> Node -> Int -> [ErrorMessage]
checkNumOutDtFlowEdgesAtLeast g n exp_num =
  let act_num = length $
                nubBy haveSameOutEdgeNrs $
                getDtFlowOutEdges g n
  in if act_num < exp_num
     then [ "Wrong number of outbound data-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected at least " ++ show exp_num
          ]
     else []

checkNumInCtrlFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumInCtrlFlowEdges g n exp_num =
  let act_num = length $ getCtrlFlowInEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of inbound control-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkNumOutCtrlFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumOutCtrlFlowEdges g n exp_num =
  let act_num = length $ getCtrlFlowOutEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of outbound control-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkNumInStFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumInStFlowEdges g n exp_num =
  let act_num = length $ getStFlowInEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of inbound state-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkNumOutStFlowEdges :: Graph -> Node -> Int -> [ErrorMessage]
checkNumOutStFlowEdges g n exp_num =
  let act_num = length $ getStFlowOutEdges g n
  in if act_num /= exp_num
     then [ "Wrong number of outbound state-flow edges: " ++ show n ++
            " has " ++ show act_num ++ ", expected " ++ show exp_num
          ]
     else []

checkHasOutStFlowEdgeOrInDefEdge :: Graph -> Node -> [ErrorMessage]
checkHasOutStFlowEdgeOrInDefEdge g n =
  let num_st_es = length $ getStFlowOutEdges g n
      num_def_es = length $ getDefInEdges g n
  in if num_st_es /= 1 && num_def_es /= 1
     then [ "Wrong number of outbound state-flow or inbound definition " ++
            "edges: " ++
            show n ++ " has " ++ show num_st_es ++ " state-flow edges and " ++
            show num_def_es ++ " definition edges, expected either " ++
            "1 state-flow edge or 1 definition edge (but not both)"
          ]
     else []

checkEntryBlock :: OpStructure -> [ErrorMessage]
checkEntryBlock os =
  let g = osGraph os
      entry_n = osEntryBlockNode os
  in if isJust entry_n
     then let nid = fromJust entry_n
          in checkNodeExists ("entry block node with ID " ++ pShow nid) g nid
     else []

checkValueLocations :: OpStructure -> [ErrorMessage]
checkValueLocations os =
  let g = osGraph os
  in concatMap ( \(nid, _) ->
                 checkNodeExists ( "valid location specification with ID " ++
                                   pShow nid
                                 )
                                 g
                                 nid
               )
               (osValidLocations os)

checkPhiDefEdges :: Graph -> Node -> [ErrorMessage]
checkPhiDefEdges g n =
  let in_es = getDtFlowInEdges g n
      out_es = getDtFlowOutEdges g n
      checkInEdge e =
        let src = getSourceNode g e
            nr = getOutEdgeNr e
            num_def_es = length $
                         filter (\e' -> getOutEdgeNr e' == nr) $
                         getDefOutEdges g src
        in if num_def_es /= 1
           then [ "Wrong number of outbound definition edges: " ++ show src ++
                  ", which is predecessor of " ++ show n ++ ", has " ++
                  pShow num_def_es ++ " definition edges with out-edge-" ++
                  "number " ++ pShow nr ++ ", expected 1"
                ]
           else []
      checkOutEdge e =
        let trg = getTargetNode g e
            num_def_es = length $ getDefInEdges g trg
        in if num_def_es /= 1
           then [ "Wrong number of inbound definition edges: " ++ show trg ++
                  ", which is successor of " ++ show n ++ ", has " ++
                  pShow num_def_es ++ " definition edges, expected 1"
                ]
           else []
      msg0 = concatMap checkInEdge in_es
      msg1 = concatMap checkOutEdge out_es
  in concat [msg0, msg1]

checkConstraints :: OpStructure -> [ErrorMessage]
checkConstraints os =
  let g = osGraph os
      cs = osConstraints os
  in concatMap (checkNodeInConstraint g) cs

checkNodeInConstraint :: Graph -> Constraint -> [ErrorMessage]
checkNodeInConstraint g c =
  checkConstraint [] c
  where
  checkConstraint msgs (BoolExprConstraint expr) =
    checkBoolExpr msgs expr
  checkBoolExpr msgs (EqExpr  lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (NEqExpr lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (GTExpr  lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (GEExpr  lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (LTExpr  lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (LEExpr  lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkBoolExpr msgs (AndExpr lhs rhs) =
    checkBoolExpr (checkBoolExpr msgs lhs) rhs
  checkBoolExpr msgs (OrExpr  lhs rhs) =
    checkBoolExpr (checkBoolExpr msgs lhs) rhs
  checkBoolExpr msgs (ImpExpr lhs rhs) =
    checkBoolExpr (checkBoolExpr msgs lhs) rhs
  checkBoolExpr msgs (EqvExpr lhs rhs) =
    checkBoolExpr (checkBoolExpr msgs lhs) rhs
  checkBoolExpr msgs (NotExpr expr) =
    checkBoolExpr msgs expr
  checkBoolExpr msgs (InSetExpr lhs rhs) =
    checkSetExpr (checkSetElemExpr msgs lhs) rhs
  checkBoolExpr msgs (FallThroughFromMatchToBlockExpr expr) =
    checkBlockExpr msgs expr
  checkNumExpr msgs (PlusExpr lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkNumExpr msgs (MinusExpr lhs rhs) =
    checkNumExpr (checkNumExpr msgs lhs) rhs
  checkNumExpr msgs (Int2NumExpr expr) =
    checkIntExpr msgs expr
  checkNumExpr msgs (Bool2NumExpr expr) =
    checkBoolExpr msgs expr
  checkNumExpr msgs (Node2NumExpr expr) =
    checkNodeExpr msgs expr
  checkNumExpr msgs (Match2NumExpr expr) =
    checkMatchExpr msgs expr
  checkNumExpr msgs (Instruction2NumExpr expr) =
    checkInstructionExpr msgs expr
  checkNumExpr msgs (Block2NumExpr expr) =
    checkBlockExpr msgs expr
  checkNumExpr msgs (Location2NumExpr expr) =
    checkLocationExpr msgs expr
  checkIntExpr msgs (AnIntegerExpr _) =
    msgs
  checkNodeExpr msgs (ANodeIDExpr nid) =
    msgs ++
    checkNodeExists ( "constraint " ++ toLispExpr c ++ " with node ID " ++
                      pShow nid
                    )
                    g
                    nid
  checkNodeExpr msgs (ANodeArrayIndexExpr _) =
    msgs
  checkNodeExpr msgs (NodeSelectedForOperandExpr expr) =
    checkOperandExpr msgs expr
  checkOperandExpr msgs (AnOperandIDExpr _) =
    msgs
  checkOperandExpr msgs (AnOperandArrayIndexExpr _) =
    msgs
  checkMatchExpr msgs (AMatchIDExpr _) =
    msgs
  checkMatchExpr msgs (AMatchArrayIndexExpr _) =
    msgs
  checkMatchExpr msgs (ThisMatchExpr) =
    msgs
  checkInstructionExpr msgs (AnInstructionIDExpr _) =
    msgs
  checkInstructionExpr msgs (AnInstructionArrayIndexExpr _) =
    msgs
  checkInstructionExpr msgs (InstructionOfMatchExpr expr) =
    checkMatchExpr msgs expr
  checkBlockExpr msgs (BlockOfBlockNodeExpr expr) =
    checkNodeExpr msgs expr
  checkLocationExpr msgs (ALocationIDExpr _) =
    msgs
  checkLocationExpr msgs (ALocationArrayIndexExpr _) =
    msgs
  checkLocationExpr msgs (LocationOfValueNodeExpr expr) =
    checkNodeExpr msgs expr
  checkLocationExpr msgs (TheNullLocationExpr) =
    msgs
  checkSetExpr msgs (UnionSetExpr lhs rhs) =
    checkSetExpr (checkSetExpr msgs lhs) rhs
  checkSetExpr msgs (IntersectSetExpr lhs rhs) =
    checkSetExpr (checkSetExpr msgs lhs) rhs
  checkSetExpr msgs (DiffSetExpr lhs rhs) =
    checkSetExpr (checkSetExpr msgs lhs) rhs
  checkSetExpr msgs (LocationClassExpr expr) =
    concatMap (checkLocationExpr msgs) expr
  checkSetElemExpr msgs (Block2SetElemExpr expr) =
    checkBlockExpr msgs expr
  checkSetElemExpr msgs (Location2SetElemExpr expr) =
    checkLocationExpr msgs expr
