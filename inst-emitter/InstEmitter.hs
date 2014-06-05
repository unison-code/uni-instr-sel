{-
Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
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
Takes the solution JSON file and the post-processing parameters JSON file and
outputs (on stdout) the corresponding assembly instructions for that solution.
-}

import qualified Language.InstructionSelection.DataTypes as D
import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Constraints.PCBuilder
import Language.InstructionSelection.CPModel.JsonDumper
import Language.InstructionSelection.CPModel.ParamMaker
import Language.InstructionSelection.Graphs
import Language.InstructionSelection.Graphs.VFTwo
import Language.InstructionSelection.TargetMachine
import Language.InstructionSelection.OpStructures
import qualified Language.InstructionSelection.OpTypes as O
import Language.InstructionSelection.Patterns
import Data.List ( mapAccumL
                 , zip5
                 )

main :: IO ()
main =
  do putStrLn "hello"
