{-
Copyright (c) 2013-2014, Gabriel Hjort Blindell <ghb@kth.se>
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{-
Tests the implementation of the VF2 algorithm.
-}

import qualified Language.InstructionSelection.DataTypes as D
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Graphs.VFTwo
import Language.InstructionSelection.OpStructures
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.CPModel
import Language.InstructionSelection.CPModel.JsonDumper
import Language.InstructionSelection.CPModel.ParamMaker
import Language.InstructionSelection.PrettyPrint

main :: IO ()
main =
  do let search = mkGraph
                  [ (0, NodeLabel 0 (NodeInfo (ComputationNode
                                               (O.UIntOp O.Sub))
                                     ""))
                  , (1, NodeLabel 1 (NodeInfo (DataNode D.UnknownType) ""))
                  , (2, NodeLabel 2 (NodeInfo (DataNode D.UnknownType) ""))
                  , (3, NodeLabel 3 (NodeInfo (DataNode D.UnknownType) ""))
                  ]
                  [ (1, 0, EdgeLabel 0 0)
                  , (2, 0, EdgeLabel 0 1)
                  , (0, 3, EdgeLabel 0 0)
                  ]
         pattern = mkGraph
                  [ (4, NodeLabel 4 (NodeInfo (ComputationNode
                                               (O.UIntOp O.Sub))
                                     ""))
                  , (5, NodeLabel 5 (NodeInfo (DataNode D.UnknownType) ""))
                  , (6, NodeLabel 6 (NodeInfo (DataNode D.UnknownType) ""))
                  , (7, NodeLabel 7 (NodeInfo (DataNode D.UnknownType) ""))
                  ]
                  [ (5, 4, EdgeLabel 0 0)
                  , (6, 4, EdgeLabel 0 1)
                  , (4, 7, EdgeLabel 0 0)
                  ]
         search_os = normalizeNodeIds $ OpStructure search []
         pattern_os = normalizeNodeIds $ OpStructure pattern []
         matchset = match search pattern
         params = mkParams search_os [(pattern_os, matchset, 0)]
     putStrLn $ toJson params
