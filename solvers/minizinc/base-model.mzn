% Copyright (c) 2013-2016, Gabriel Hjort Blindell <ghb@kth.se>
%                          Mats Carlsson <matsc@sics.se>
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
% 1. Redistributions of source code must retain the above copyright notice,
%    this list of conditions and the following disclaimer.
% 2. Redistributions in binary form must reproduce the above copyright notice,
%    this list of conditions and the following disclaimer in the documentation
%    and/or other materials provided with the distribution.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.



%=====================
% EXTERNAL PARAMETERS
%=====================

% Function data.
int: numOperationsInFunction;
int: numDataInFunction;
int: numBlocksInFunction;
int: entryBlockOfFunction;
array[allBlocksInFunction] of set of int: domSetOfBlockInFunction;
array[int, int] of int: funValidDataLocs;
set of allOperationsInFunction: copiesInFunction;
set of allDataInFunction: statesInFunction;
array[int, int] of int: stateDefEdgesForBlocksInFunction;
array[allBlocksInFunction] of int: execFrequencyOfBlockInFunction;

% Target machine data.
int: numLocations;

% Match data.
int: numMatches;
int: numOperands;
array[allOperands] of set of int: operandAlternatives;
array[allMatches] of set of int: operationsCoveredByMatch;
array[allMatches] of set of int: operandsDefinedByMatch;
array[allMatches] of set of int: operandsUsedByMatch;
array[allMatches] of set of int: operandsExternalToMatch;
array[allMatches] of set of int: operandsInternalToMatch;
array[allMatches] of set of int: entryBlockOfMatch;
array[allMatches] of set of int: spannedBlocksInMatch;
array[allMatches] of set of int: consumedBlocksInMatch;
array[int, int] of int: matchValidDataLocs;
array[int, int] of int: inputDefEdgesInMatch;
array[int, int] of int: outputDefEdgesInMatch;
array[allMatches] of int: codeSizeOfMatch;
array[allMatches] of int: latencyOfMatch;
set of allMatches: copyInstrMatches;
set of allMatches: inactiveCopyInstrMatches;
set of allMatches: nullInstrMatches;
set of allMatches: phiInstrMatches;
array[allMatches] of allMatches: allMatchesBySize;
set of allMatches: dominatedMatches;

% Arrays that encode constraints
array[int, int] of int: sameLoc;
array[int, int] of int: inBlockSucc;
array[int, int] of int: inBlock;
array[int, int] of int: matchValidDataLocRanges;
array[int, int] of int: funValidDataLocRanges;

% These variables will be set by concatenating the definitions to the end of
% this file.



%=====================
% INTERNAL PARAMETERS
%=====================

% Total number of location values (an additional value will be needed for
% representing the null location, for when the datum cannot be reused by other
% matches).
int: numLocValues = numLocations + 1;

% Reference to to the null-location value.
int: locValueForNull = numLocValues - 1;

% Total number of operand values (an additional value will be needed for
% representing the null datum, for operands used in non-selected matches).
int: numOperandValues = numOperands + 1;

% Reference to to the null-datum value.
int: opValueForNull = numOperandValues - 1;

% Total number of blocks values (an additional value will be needed for
% representing the null block, to which non-selected matches will be placed).
int: numBlockValues = numBlocksInFunction + 1;

% Reference to the null-block value.
int: blockValueForNull = numBlockValues - 1;

% Extends the external parameter 'domSetOfBlockInFunction' with a value
% for the null block (which is assumed to be dominated by all blocks).
array[allBlocksInFunctionPlusNull] of set of int:
  domSetOfBlockInFunctionPlusNull =
    array1d( allBlocksInFunctionPlusNull
           , domSetOfBlockInFunction
             ++
             array1d( blockValueForNull..blockValueForNull,
                      [allBlocksInFunction]
                    )
           );

% Extends the external parameter 'execFrequencyOfBlockInFunction' with a value
% of 0 for the null block.
array[allBlocksInFunctionPlusNull] of int: execFrequencyOfBlocksPlusNull =
  array1d( allBlocksInFunctionPlusNull
         , execFrequencyOfBlockInFunction
           ++
           array1d(blockValueForNull..blockValueForNull, [0])
         );

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 0..numOperationsInFunction-1;
set of int: allDataInFunction = 0..numDataInFunction-1;
set of int: allBlocksInFunction = 0..numBlocksInFunction-1;
set of int: allBlocksInFunctionPlusNull = 0..numBlockValues-1;
set of int: allMatches = 0..numMatches-1;
set of int: allOperands = 0..numOperands-1;
set of int: allOperandsPlusNull = 0..numOperandValues-1;
set of int: allLocValues = 0..numLocValues-1;



%===========
% VARIABLES
%===========

% Data definitions, locations, and activity.
array[allDataInFunction] of var allBlocksInFunction: def;
array[allDataInFunction] of var allLocValues: loc;
array[allDataInFunction] of var bool: active;

% Match selection.
array[allMatches] of var bool: sel;

% Blocks wherein the matches are placed.
array[allMatches] of var allBlocksInFunctionPlusNull: place;

% Data selected for the operands.
array[allOperandsPlusNull] of var allDataInFunction: alt;

% Block ordering (succ[b] is the block appearing immeditely after block b in the
% generated code).
array[allBlocksInFunctionPlusNull] of var allBlocksInFunctionPlusNull: succ;

% Cost.
var int: cost;



%====================
% GLOBAL CONSTRAINTS
%====================

include "all_equal.mzn";
include "count.mzn";
include "circuit.mzn";
include "table.mzn";



%============
% PREDICATES
%============

% Fall-through constraint: the block wherein match m is placed must be
% immediately followed by block b.
predicate fall_through(int: m, int: b) =
(
  succ[place[m]] = b
);



%=============
% CONSTRAINTS
%=============

% Dominated matches must not be selected.
constraint
  forall (m in dominatedMatches)
  (
    not sel[m]
  );

% Constrain alternatives of operands.
constraint
  forall (o in allOperands)
  (
    alt[o] in (operandAlternatives[o] union {opValueForNull})
  );

% Constrain locations of data representing values.
constraint
  forall (d in allDataInFunction)
  (
    let {
      set of int: locs = { funValidDataLocs[i, 2]
                         | i in index_set_1of2(funValidDataLocs)
                                where funValidDataLocs[i, 1] = d
                         }
    }
    in if card(locs) > 0
       then loc[d] in locs
       else true
       endif
  );

% Enforce that, for each operation, exactly one match must be selected such that
% the operation is covered.
constraint
  forall (o in allOperationsInFunction)
  (
    let {
      set of int: mset = { m | m in allMatches diff dominatedMatches
                                    where o in operationsCoveredByMatch[m]
                         }
    }
    in if card(mset) = 1
       then sel[min(mset)]
       else if card(mset) = 2
            then sel[min(mset)] xor sel[max(mset)]
            else sum (m in mset)
                 (
                   bool2int(sel[m])
                 ) = 1
            endif
       endif
  );

% A state datum with a definition edge with a block must be defined in the block
% of that block.
constraint
  forall (i in index_set_1of2(stateDefEdgesForBlocksInFunction))
  (
    let {
      int: b = stateDefEdgesForBlocksInFunction[i, 1]
    , int: d = stateDefEdgesForBlocksInFunction[i, 2]
    }
    in def[d] = b
  );

% Enforce that, for each datum, exactly one match must be selected such that
% the datum is defined.
%
% This is an implied constraint, but it also enforces that the patterns for
% defining the function input and constants are selected. Such patterns do not
% cover any operations, they are not entailed in the above constraint for
% exactly covering each operation.
constraint
  forall (d in allDataInFunction)
  (
    let {
      set of int: mset = { m | m in allMatches diff dominatedMatches
                             , o in operandsDefinedByMatch[m]
                                    where d in operandAlternatives[o]
                         }
    }
    in if card(mset) = 1
       then sel[min(mset)]
       else if card(mset) = 2
            then sel[min(mset)] xor sel[max(mset)]
            else sum (m in mset)
                 (
                   bool2int(sel[m])
                 ) = 1
            endif
       endif
  );

% Operands in non-selected matches are set to the null datum.
constraint
  forall (m in allMatches)
  (
    forall (o in operandsDefinedByMatch[m] union operandsUsedByMatch[m])
    (
      % mzn-cpx crashes if posting "sel[m] <-> (alt[o] != opValueForNull)"!
      sel[m] -> (alt[o] != opValueForNull)
      /\
      (alt[o] == opValueForNull) -> (not sel[m])
    )
  );

% A datum is active if there is at least one operand that uses that datum.
constraint
  forall (d in allDataInFunction)
  (
    % mzn-cpx crashes if <-> is used in this situation
    ( active[d]
      ->
      exists (o in allOperands)
      (
        alt[o] == d
      )
    )
    /\
    ( forall (d in allDataInFunction)
      (
        forall (o in allOperands)
        (
          alt[o] != d
        ) -> not active[d]
      )
    )
  );

% Inactive data must be defined by inactive copy matches.
constraint
  forall (d in allDataInFunction)
  (
    let {
      set of int: mset = { m | m in allMatches diff dominatedMatches
                             , o in operandsDefinedByMatch[m]
                                    where d in operandAlternatives[o]
                         }
    }
    in (not active[d]) <-> sel[min(mset)]
  );

% Implied: The total number of data defined by the selected matches must be
% equal to the number of data in the function graph (Mats calls this
% "minimal effect").
constraint
  sum (m in allMatches diff dominatedMatches)
  (
    card(operandsDefinedByMatch[m]) * bool2int(sel[m])
  ) = numDataInFunction;

% Selected matches must not be placed in the null block.
constraint
  forall (m in allMatches)
  (
    sel[m] <-> place[m] != blockValueForNull
  );

% Selected matches that have an entry block must be placed in entry block.
constraint
  forall (m in allMatches diff dominatedMatches)
  (
    % If a match has no entry block, then this set will be empty and hence there
    % will be no such constraint. It is assumed that there will be at most one
    % entry.
    forall (r in entryBlockOfMatch[m])
    (
      place[m] in {r, blockValueForNull}
    )
  );

% Data defined by a selected match must be defined in either the block wherein
% the match is placed or in one of the blocks spanned by the match.
constraint
  forall (m in allMatches diff dominatedMatches)
  (
    forall (o in operandsDefinedByMatch[m])
    (
      if (card(spannedBlocksInMatch[m]) > 0)
      then
        sel[m] -> def[alt[o]] in spannedBlocksInMatch[m]
      else
        sel[m] -> def[alt[o]] = place[m]
      endif
    )
  );

% No selected matches may be placed in a block which is consumed by some
% selected match.
constraint
  forall (m, mm in allMatches, b in consumedBlocksInMatch[m])
  (
    sel[m] -> place[mm] != b
  );

% For every block wherein a datum d is defined, there must exist some selected
% match that defines d such that it is either placed in that block or that block
% is part of one of the blocks that appear in the selected match.
%
% I am not certain whether this is a strictly required or just an implied
% constraint...
constraint
  forall (d in allDataInFunction, l in allBlocksInFunction)
  (
    def[d] = l
    ->
    exists ( m in allMatches diff dominatedMatches
           , o in operandsDefinedByMatch[m]
                  where d in operandAlternatives[o]
           )
    (
      alt[o] = d
      /\
      (place[m] = l \/ (sel[m] /\ l in spannedBlocksInMatch[m]))
    )
  );

% Enforce that every datum is defined in a block such that the block dominates
% all blocks wherein the datum is used. This constraint shall not be applied to
% the generic phi patterns.
%
% The code below is essentially a more efficient implementation of (it is okay
% to apply this constraint even on non-selected matches as these will be placed
% in the null block, which is dominated by every other block in the function):
%     constraint
%       forall ( m in allMatches
%              , o in operandsUsedByMatch[m]
%                     where not (m in phiInstrMatches)
%              )
%       (
%         def[alt[o]] in domSetOfBlockInFunctionPlusNull[place[m]]
%       );
%
constraint
  forall ( m in allMatches,
           o in operandsUsedByMatch[m]
                where not (m in phiInstrMatches)
         )
  (
    def[alt[o]] in domSetOfBlockInFunctionPlusNull[place[m]]
  );
%
% TODO: FIX so that it supports alternatives
%
%int: DomRelSize =
%  sum (l in allBlocksInFunction)
%  (
%    card(domSetOfBlockInFunction[l])
%  ) + numBlocksInFunction;
%array[1..DomRelSize, 1..2] of allBlocksInFunctionPlusNull: DomRel =
%  array2d(1..DomRelSize, 1..2,
%    [ if k=1 then i else j endif | i in allBlocksInFunctionPlusNull,
%                                   j in domSetOfBlockInFunctionPlusNull[i],
%                                   k in 1..2
%    ]);
%constraint
%  forall ( m in allMatches diff dominatedMatches, d in dataUsedByMatch[m]
%                where applyDefDomUseConstraintForMatch[m]
%         )
%  (
%    table([place[m], def[d]], DomRel)
%  );

% If a match representing a phi instruction is selected, then its data
% must be defined in the blocks indicated by the definition edges.
%
% Input data d must be placed in some block b that dominates the block b'
% indicated by the definition d -> b'. However, no two data d1 or d2 must be
% placed in the same block that dominates both blocks of d1 and d2. The output
% data must be placed in the block indicated by the definition edge.
%
% Input data constraints
constraint
  forall (m in phiInstrMatches)
  (
    let {
      set of int: I = index_set_1of2(inputDefEdgesInMatch)
    , array[int] of int: oset = [ inputDefEdgesInMatch[i, 3]
                                | i in I
                                , b in allBlocksInFunction
                                       where inputDefEdgesInMatch[i, 1] = m
                                             /\
                                             inputDefEdgesInMatch[i, 2] = b
                                ]
    , array[int] of int: bset = [ inputDefEdgesInMatch[i, 2]
                                | i in I
                                       where inputDefEdgesInMatch[i, 1] = m
                                ]
    }
    in forall (i,j in 1..length(oset) where i != j)
       (
         (sel[m] -> (def[alt[oset[i]]] in domSetOfBlockInFunction[bset[i]]))
         /\
         (sel[m] -> (def[alt[oset[j]]] in domSetOfBlockInFunction[bset[j]]))
         /\
         ( sel[m] ->
           (
             sum (k in [i, j])
             (
               bool2int
               (
                 def[alt[oset[k]]]
                 in
                 ( domSetOfBlockInFunction[bset[i]]
                   intersect
                   domSetOfBlockInFunction[bset[j]]
                 )
               )
             ) <= 1
           )
         )
       )
  );
% Output data constraints
constraint
  forall (i in index_set_1of2(outputDefEdgesInMatch))
  (
    let {
      int: m = outputDefEdgesInMatch[i, 1]
    , int: b = outputDefEdgesInMatch[i, 2]
    , int: o = outputDefEdgesInMatch[i, 3]
    }
    in sel[m] -> def[alt[o]] = b
  );

% Constrain locations of operands representing values for selected matches.
constraint
  forall (m in allMatches)
  (
    forall (o in allOperands)
    (
      let {
        set of int: locs = { matchValidDataLocs[i, 3]
                           | i in index_set_1of2(matchValidDataLocs)
                                  where matchValidDataLocs[i, 1] = m
                                  /\
                                  matchValidDataLocs[i, 2] = o
                           }
      }
      in if card(locs) > 0
         then sel[m] -> loc[alt[o]] in locs
         else true
         endif
    )
  );

% If a match representing a phi instruction is selected, then its data
% must be placed in the same location.
constraint
  forall (m in phiInstrMatches)
  (
    sel[m] -> all_equal([ loc[alt[o]] | o in operandsExternalToMatch[m] ])
  );

% If a match representing a null instruction is selected, then the data used by
% the match must be defined in the same block wherein the match is placed.
% However, this does not apply to inactive copy instructions.
constraint
  forall (m in nullInstrMatches where not (m in inactiveCopyInstrMatches))
  (
    forall (o in operandsUsedByMatch[m])
    (
      sel[m] -> def[alt[o]] = place[m]
    )
  );

% Ensure that succ forms a circuit (thus resulting in an ordering of blocks).
constraint
  circuit(succ) :: domain;

% The block of the entry block (i.e. function entry point) must be placed as the
% first block, and the block of the null block must be placed as the last block.
constraint
  succ[blockValueForNull] = entryBlockOfFunction;

% All data external to a match, and which are not states, must not be located in
% the null location.
constraint
  forall ( m in allMatches diff dominatedMatches
         , o in operandsExternalToMatch[m]
                where not (operandAlternatives[o] subset statesInFunction)
         )
  (
    sel[m] -> loc[alt[o]] != locValueForNull
  );

% All data internal to a match must be located in the null location.
constraint
  forall ( m in allMatches diff dominatedMatches
         , o in operandsInternalToMatch[m]
                where not (operandAlternatives[o] subset statesInFunction)
         )
  (
    sel[m] -> loc[alt[o]] = locValueForNull
  );

% Constrain the loc value for all data that are states.
constraint
  forall (d in statesInFunction)
  (
    loc[d] = locValueForNull
  );

% Accumulate cost of selected patterns.
% TODO: make this part generic
constraint
  cost = sum (m in allMatches diff dominatedMatches)
         ( latencyOfMatch[m]
         * execFrequencyOfBlocksPlusNull[place[m]]
         );



%===========================
% PARAMETERIZED CONSTRAINTS
%===========================

constraint
  forall(i in index_set_1of2(sameLoc))(
    let {int: m = sameLoc[i,1],
         int: p = sameLoc[i,2],
         int: q = sameLoc[i,3]} in
	sel[m] -> loc[p] = loc[q]
  );

constraint
  forall(i in index_set_1of2(inBlockSucc))(
    let {int: m = inBlockSucc[i,1],
         int: p = inBlockSucc[i,2],
         int: q = inBlockSucc[i,3]} in
	place[m] in {p,blockValueForNull} /\
	(sel[m] -> succ[p] = q)
  );

constraint
  forall(i in index_set_1of2(inBlock))(
    let {int: m = inBlock[i,1],
         int: p = inBlock[i,2]} in
	place[m] in {p,blockValueForNull}
  );

constraint
  forall(i in index_set_1of2(matchValidDataLocRanges))(
    let {int: m = matchValidDataLocRanges[i,1],
         int: o = matchValidDataLocRanges[i,2]} in
    sel[m] -> loc[alt[o]]
              in matchValidDataLocRanges[i,3]..matchValidDataLocRanges[i,4]
  );

constraint
  forall(i in index_set_1of2(funValidDataLocRanges))(
    let {int: d = funValidDataLocRanges[i,1]} in
	loc[d] in funValidDataLocRanges[i,2]..funValidDataLocRanges[i,3]
  );



%======================================
% IMPLIED BY PARAMETERIZED CONSTRAINTS
%======================================

% Two matches can't both be selected, if they imply conflicting successor blocks.
constraint
  forall(i in index_set_1of2(inBlockSucc), j in index_set_1of2(inBlockSucc) where i<j)(
    let {int: mi = inBlockSucc[i,1],
         int: pi = inBlockSucc[i,2],
         int: qi = inBlockSucc[i,3],
	 int: mj = inBlockSucc[j,1],
         int: pj = inBlockSucc[j,2],
         int: qj = inBlockSucc[j,3]} in
	(pi=pj xor qi=qj) -> (not sel[mi] \/ not sel[mj])
  );

% Two matches can't both be selected, if the first implies that two locations
% are equal, and the second implies that the intersection of their domains is
% empty
constraint
  forall(i in index_set_1of2(sameLoc))(
    let {int: m1 = sameLoc[i,1],
         int: p = sameLoc[i,2],
         int: q = sameLoc[i,3]} in
    forall(j1 in index_set_1of2(matchValidDataLocRanges) where matchValidDataLocRanges[j1,2]=p)(
      forall(j2 in index_set_1of2(matchValidDataLocRanges) where matchValidDataLocRanges[j2,1]=matchValidDataLocRanges[j1,1] /\
                                                   matchValidDataLocRanges[j2,2]=q)(
        let {int: m2 = matchValidDataLocRanges[j1,1]} in
	    card((matchValidDataLocRanges[j1,3]..matchValidDataLocRanges[j1,4]) intersect (matchValidDataLocRanges[j2,3]..matchValidDataLocRanges[j2,4]))=0 ->
            not sel[m1] \/ not sel[m2]
      )
    )
  );

% TODO: FIX!
%
% Ad hoc: detect symmetry among location values 1..31
%
%constraint
%  forall(i in index_set_1of2(funValidDataLocRanges))(
%    let {int: lo = funValidDataLocRanges[i,2],
%         int: hi = funValidDataLocRanges[i,3]} in
%	lo<=31 -> hi>=31 \/ hi=0
%  ) /\
%  forall(i in index_set_1of2(matchValidDataLocRanges))(
%    let {int: lo = matchValidDataLocRanges[i,3],
%         int: hi = matchValidDataLocRanges[i,4]} in
%	lo<=31 -> hi>=31 \/ hi=0
%  ) ->
%  forall(i in allDataInFunction)(loc[i] in {0} union (31..numLocValues-1));



%==================
% SOLVE AND OUTPUT
%==================

solve ::
  seq_search(
    [ bool_search([ sel[m]
                  | m in allMatchesBySize
                    where not ( m in copyInstrMatches
                                     union inactiveCopyInstrMatches
                                     union dominatedMatches
                              )
                  ], input_order, indomain_max, complete)
    , int_search(alt, input_order, indomain_max, complete)
	]
  )
minimize cost;

output [ "sel=", show(sel), "\n"
       , "alt=", show(alt), "\n"
       , "def=", show(def), "\n"
       , "loc=", show(loc), "\n"
       , "place=", show(place), "\n"
       , "succ=", show(succ), "\n"
       , "block_value_for_null=", show(blockValueForNull), "\n"
       , "loc_value_for_null=", show(locValueForNull), "\n"
       , "op_value_for_null=", show(opValueForNull), "\n"
       , "cost=", show(cost), "\n"
       ];
