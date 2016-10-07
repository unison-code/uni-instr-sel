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
import Language.InstrSel.Functions
  ( Function (..) )
import Language.InstrSel.Graphs
import Language.InstrSel.OpStructures
  ( OpStructure (..) )
import Language.InstrSel.OpTypes
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , errorExitCode
  )

import Data.List
  ( intercalate
  , nubBy
  )



-------------
-- Functions
-------------

run
  :: CheckAction
  -> Maybe Function
  -> Maybe InstrPattern
  -> IO [Output]

run CheckFunctionIntegrity (Just fun) _ =
  do let g = osGraph $ functionOS fun
     return $ mkOutput $
              checkGraphInvariants g

run CheckPatternIntegrity _ (Just pat) =
  do let g = osGraph $ patOS pat
     return $ mkOutput $
              checkGraphInvariants g

run _ _ _ = reportErrorAndExit "CheckIntegrity: unsupported action"

-- | Makes an 'Output' from a given output log. An empty log indicates no
-- errors.
mkOutput :: [String] -> [Output]
mkOutput [] = [toOutput ""]
mkOutput msgs =
  [toOutputWithExitCode errorExitCode (intercalate "\n\n" msgs)]

checkGraphInvariants :: Graph -> [String]
checkGraphInvariants g =
  checkOperationNodes g
--  checkValueNodes g ++
--  checkStateNodes g
--  checkBlockNodes g ++
--  checkControlNodes g

checkOperationNodes :: Graph -> [String]
checkOperationNodes g =
  let ns = filter isOperationNode $
           getAllNodes g
      check n =
        case (getNodeType n) of
          (ComputationNode op) ->
            let msg0 = checkNumOperands g n op
                msg1 = checkOperandsAreValues g n
                msg2 = checkNumProducts g n op
                msg3 = checkProductsAreValues g n
                msg4 = checkStates g n op
            in concat [msg0, msg1, msg2, msg3, msg4]
            -- TODO: implement
          (ControlNode op) ->
            let msg0 = if op /= Ret
                       then checkNumOperands g n op
                       else []
                msg1 = checkOperandsAreValues g n
                msg2 = checkNumProducts g n op
                msg3 = checkProductsAreValues g n
            in concat [msg0, msg1, msg2, msg3]
            -- TODO: implement
          (CallNode {}) ->
            -- TODO: implement
            []
          (ValueNode {}) ->
            -- TODO: implement
            []
          (BlockNode {}) ->
            -- TODO: implement
            []
          PhiNode ->
            -- TODO: implement
            []
          StateNode ->
            -- TODO: implement
            []
          CopyNode ->
            -- TODO: implement
            []
  in concat $
     foldr (\n msgs -> (check n:msgs)) [] ns

checkNumOperands :: (OpType o) => Graph -> Node -> o -> [String]
checkNumOperands g n op =
  let act_num_ops = length $
                    nubBy haveSameInEdgeNrs $
                    getDtFlowInEdges g n
      exp_num_ops = numOperands op
  in if act_num_ops /= exp_num_ops
     then [ "Wrong number of operands: " ++ show n ++
            " has " ++ show act_num_ops ++ ", expected " ++ show exp_num_ops
          ]
     else []

checkOperandsAreValues :: Graph -> Node -> [String]
checkOperandsAreValues g n =
  let es = getDtFlowInEdges g n
      check e = let src = getSourceNode g e
                in if not (isValueNode src)
                   then [ "Invalid node type: " ++ show n ++ " has " ++
                          show src ++ " as operand, expected value node"
                        ]
                   else []
  in concatMap check es

checkNumProducts :: (OpType o) => Graph -> Node -> o -> [String]
checkNumProducts g n op =
  let act_num_res = length $
                    getDtFlowOutEdges g n
      exp_num_res = if producesValue op then 1 else 0
  in if act_num_res /= exp_num_res
     then [ "Wrong number of products: " ++ show n ++
            " has " ++ show act_num_res ++ ", expected " ++ show exp_num_res
          ]
     else []

checkProductsAreValues :: Graph -> Node -> [String]
checkProductsAreValues g n =
  let es = getDtFlowOutEdges g n
      check e = let trg = getTargetNode g e
                in if not (isValueNode trg)
                   then [ "Invalid node type: " ++ show n ++ " has " ++
                          show trg ++ " as product, expected value node"
                        ]
                   else []
  in concatMap check es

checkStates :: (OpType o) => Graph -> Node -> o -> [String]
checkStates g n op =
  let in_edges = getStFlowInEdges g n
      out_edges = getStFlowOutEdges g n
      act_num_in_sts = length in_edges
      exp_num_in_sts = if requiresState op then 1 else 0
      act_num_out_sts = length out_edges
      exp_num_out_sts = if requiresState op then 1 else 0
      msg0 = if act_num_in_sts /= exp_num_in_sts
             then [ "Wrong number of input states: " ++ show n ++
                    " has " ++ show act_num_in_sts ++ ", expected " ++
                    show exp_num_in_sts
                  ]
             else []
      msg1 = if act_num_out_sts /= exp_num_out_sts
             then [ "Wrong number of output states: " ++ show n ++
                    " has " ++ show act_num_out_sts ++ ", expected " ++
                    show exp_num_out_sts
                  ]
             else []
      checkInState e =
        let src = getSourceNode g e
        in if not (isStateNode src)
           then [ "Invalid node type: " ++ show n ++ " has " ++
                  show src ++ " as input, expected state node"
                ]
           else []
      checkOutState e =
        let trg = getTargetNode g e
        in if not (isStateNode trg)
           then [ "Invalid node type: " ++ show n ++ " has " ++
                  show trg ++ " as output, expected state node"
                ]
           else []
      msg2 = concatMap checkInState in_edges
      msg3 = concatMap checkOutState out_edges
  in concat [msg0, msg1, msg2, msg3]
