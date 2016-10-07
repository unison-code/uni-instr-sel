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
import Language.InstrSel.PrettyShow
import Language.InstrSel.TargetMachines
  ( InstrPattern (..) )

import Language.InstrSel.Utils.IO
  ( reportErrorAndExit
  , errorExitCode
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
mkOutput msgs = [toOutputWithExitCode errorExitCode (concatMap (++ "\n") msgs)]

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
      check _ msgs = msgs
  in foldr check [] ns
