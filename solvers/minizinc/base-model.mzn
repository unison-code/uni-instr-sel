%
%  Main authors:
%    Gabriel Hjort Blindell <ghb@kth.se>
%    Mats Carlsson <matsc@sics.se>
%
%  Contributing authors:
%    Roberto Castaneda Lozano <rcas@sics.se>
%
%  Copyright (c) 2012-2016, Gabriel Hjort Blindell <ghb@kth.se>
%  All rights reserved.
%
%  Redistribution and use in source and binary forms, with or without
%  modification, are permitted provided that the following conditions are met:
%  1. Redistributions of source code must retain the above copyright notice,
%     this list of conditions and the following disclaimer.
%  2. Redistributions in binary form must reproduce the above copyright notice,
%     this list of conditions and the following disclaimer in the documentation
%     and/or other materials provided with the distribution.
%  3. Neither the name of the copyright holder nor the names of its contributors
%     may be used to endorse or promote products derived from this software
%     without specific prior written permission.
%
%  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER BE LIABLE FOR ANY
%  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
%  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



%=====================
% EXTERNAL PARAMETERS
%=====================

% Function data.
int: numOperationsInFunction;
int: numDataInFunction;
int: numBlocksInFunction;
int: entryBlockOfFunction;
int: execFrequencyGCD;
array[allBlocksInFunction] of set of int: domSetOfBlockInFunction;
array[int, int] of int: funValidDataLocs;
set of allOperationsInFunction: copiesInFunction;
set of allDataInFunction: statesInFunction;
array[int, int] of int: stateDefEdgesForBlocksInFunction;
array[allBlocksInFunction] of int: execFrequencyOfBlockInFunction;
set of allBlocksInFunction: branchExtBlocksInFunction;

% Target machine data.
int: numLocations;
set of int: canonicalDataLocs;

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
set of allMatches: inactiveInstrMatches;
set of allMatches: nullInstrMatches;
set of allMatches: phiInstrMatches;
array[allMatches] of allMatches: allMatchesBySize;

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

% Total number of data values (an additional value will be needed for
% representing the null datum, for operands used in non-selected matches).
int: numDataValues = numDataInFunction + 1;

% Reference to to the null-datum value.
int: datumValueForNull = numDataValues;

% Total number of blocks values (an additional value will be needed for
% representing the null block, to which non-selected matches will be placed).
int: numBlockValues = numBlocksInFunction + 1;

% Reference to the null-block value.
int: blockValueForNull = numBlockValues;

% Extends the external parameter 'execFrequencyOfBlockInFunction' with a value
% of 0 for the null block.
array[allBlocksInFunctionPlusNull] of int: execFrequencyOfBlocksPlusNull =
  array1d( allBlocksInFunctionPlusNull
         , execFrequencyOfBlockInFunction
           ++ array1d(blockValueForNull..blockValueForNull, [0])
         );

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 1..numOperationsInFunction;
set of int: allDataInFunction = 1..numDataInFunction;
set of int: allBlocksInFunction = 1..numBlocksInFunction;
set of int: allBlocksInFunctionPlusNull = 1..numBlockValues;
set of int: allMatches = 1..numMatches;
set of int: allOperands = 1..numOperands;
set of int: allLocValues = 0..numLocValues-1;



%===========
% VARIABLES
%===========

% Data definitions, locations, and activity.
array[allDataInFunction] of var allBlocksInFunction: def;
array[allDataInFunction] of var canonicalDataLocs: loc;

% Match selection.
array[allMatches] of var bool: sel;

% Blocks wherein the matches are placed.
array[allMatches] of var allBlocksInFunctionPlusNull: place;

% Data selected for the operands.
array[allOperands] of var allDataInFunction: alt;

% Block ordering (succ[b] is the block appearing immeditely after block b in the
% generated code).
array[allBlocksInFunctionPlusNull] of var allBlocksInFunctionPlusNull: succ;

% Cost.
var int: cost;

% [MC] Dual variables.
array[allOperationsInFunction] of set of allMatches: omatchD =
  [ { m | m in allMatches
            where op in operationsCoveredByMatch[m]}
  | op in allOperationsInFunction
  ];
array[allDataInFunction] of set of allMatches: dmatchD =
  [ { m | m in allMatches
        , o in operandsDefinedByMatch[m]
            where d in operandAlternatives[o]}
  | d in allDataInFunction
  ];
array[int] of set of allMatches: dmatchDOnly =
  [ M | M in dmatchD
          where forall(i in index_set(omatchD))(omatchD[i] != M)
  ];

array[allMatches] of int: opOfM =
  [ min({op | op in allOperationsInFunction where m in omatchD[op]})
  | m in allMatches
  ];
array[allOperationsInFunction] of var allMatches: omatch;
array[allOperationsInFunction] of var allBlocksInFunction: oplace;
array[allDataInFunction] of var allMatches: dmatch;



%====================
% GLOBAL CONSTRAINTS
%====================

include "circuit.mzn";
include "table.mzn";



%===========
% FUNCTIONS
%===========

% Bypasses the alt[.] variable if the given operand only has one alternative.
% Valid under the assumption that its operation was selected.
var allDataInFunction: Alt(allOperands: o) =
  let
  { allDataInFunction: d = min(operandAlternatives[o])
  } in
  if card(operandAlternatives[o]) = 1 then d else alt[o] endif;



%=============
% CONSTRAINTS
%=============

% Constrain alternatives of operands.
constraint
  forall (o in allOperands) (
    alt[o] in operandAlternatives[o]
  );

% Constrain locations of data representing values.
constraint
  forall (d in allDataInFunction) (
    let
    { set of int: locs = { funValidDataLocs[i, 2]
                         | i in index_set_1of2(funValidDataLocs)
                           where funValidDataLocs[i, 1] = d
                         }
    } in
    if card(locs) > 0 then loc[d] in locs else true endif
  );

% [MC] Dual variables: for each operation, exactly one match must be selected
% such that the operation is covered.
constraint
  forall (o in allOperationsInFunction) (
    omatch[o] in omatchD[o] /\
    forall (m in omatchD[o]) (
      omatch[o] = m <-> sel[m]
    )
  );

% [MC] Dual variables: for each datum, exactly one match must be selected such
% that the datum is defined.
constraint
  forall (d in allDataInFunction) (
    dmatch[d] in dmatchD[d] /\
    forall (m in dmatchD[d]) (
      dmatch[d] = m <-> sel[m]
    )
  );

% A state datum with a definition edge with a block must be defined in the block
% of that block.
constraint
  forall (i in index_set_1of2(stateDefEdgesForBlocksInFunction)) (
    let
    { int: b = stateDefEdgesForBlocksInFunction[i, 1]
    , int: d = stateDefEdgesForBlocksInFunction[i, 2]
    } in
    def[d] = b
  );

% Fix operand value if match was not selected.
constraint
  forall (m in allMatches) (
    forall (o in operandsDefinedByMatch[m] union operandsUsedByMatch[m]
            where card(operandAlternatives[o])>1) (
      not sel[m] -> alt[o] = min(operandAlternatives[o])
    )
  );

% [MC] A datum is active if at least one operand uses that datum.
% Active data must not be defined by inactive copy matches.
constraint
  forall (mi in inactiveInstrMatches,
	  oi in operandsDefinedByMatch[mi],
	  d in operandAlternatives[oi]) (
    exists (m in allMatches,
            o in operandsUsedByMatch[m]
	    where d in operandAlternatives[o])(sel[m] /\ alt[o]=d)
    <->
    not sel[mi]
  );

% [GHB] These constraints have been reformulated by MC into a table constraint.
%
% % Selected matches must not be placed in the null block.
% constraint
%   forall (m in allMatches) (
%     sel[m] <-> place[m] != blockValueForNull
%   );
%
% % Selected matches that have an entry block must be placed in entry block.
% constraint
%   forall (m in allMatches) (
%     % If a match has no entry block, then this set will be empty and hence
%     % there will be no such constraint. It is assumed that there will be at
%     % most one entry.
%     forall (r in entryBlockOfMatch[m]) (
%       place[m] in {r, blockValueForNull}
%     )
%   );
%
% % Data defined by a selected match must be defined in either the block wherein
% % the match is placed or in one of the blocks spanned by the match.
% constraint
%   forall (m in allMatches) (
%     forall (o in operandsDefinedByMatch[m]) (
%       if (card(spannedBlocksInMatch[m]) > 0)
%       then sel[m] -> def[Alt(o)] in spannedBlocksInMatch[m]
%       else sel[m] -> def[Alt(o)] = place[m]
%       endif
%     )
%   );

% No selected matches may be placed in a block which is consumed by some
% selected match.
constraint
  forall (m, mm in allMatches, b in consumedBlocksInMatch[m]) (
    sel[m] -> place[mm] != b
  );

% No data may be defined in a branch-extension block.
constraint
  forall (b in branchExtBlocksInFunction) (
    forall (d in allDataInFunction) (
      def[d] != b
    )
  );

% No matches that does not span any blocks may be placed in a branch-extension
% block.
constraint
  forall (b in branchExtBlocksInFunction) (
    forall (m in allMatches where not (b in spannedBlocksInMatch[m])) (
      place[m] != b
    )
  );

% Enforce that every datum is defined in a block such that the block dominates
% all blocks wherein the datum is used. This constraint shall not be applied to
% the generic phi patterns.
%
% [GHB] Used to be:
%   constraint
%     forall ( m in allMatches,
%              o in operandsUsedByMatch[m]
%              where not (m in phiInstrMatches)
%            )
%     (
%       def[alt[o]] in domSetOfBlockInFunctionPlusNull[place[m]]
%     );
% Also, selected matches that have an entry block must be placed in entry block.

int: DomRelSize =
 sum (l in allBlocksInFunction)
 (
   card(domSetOfBlockInFunction[l])
 );
array[1..DomRelSize, 1..2] of allBlocksInFunction: DomRel =
 array2d(1..DomRelSize, 1..2,
   [ if k=1 then i else j endif | i in allBlocksInFunction,
                                  j in domSetOfBlockInFunction[i],
                                  k in 1..2
   ]);

constraint
  forall (m in allMatches diff phiInstrMatches
          where card(spannedBlocksInMatch[m])=0) (
    let {array[int] of int: defs = [x | x in operandsDefinedByMatch[m]],
	 array[int] of int: usonly = [x | x in operandsUsedByMatch[m] diff operandsDefinedByMatch[m]],
	 array[index_set(defs)] of var allBlocksInFunctionPlusNull: defplaces,
	 array[index_set(usonly)] of var allBlocksInFunctionPlusNull: useplaces,
	 array[int] of var allBlocksInFunctionPlusNull: places = defplaces ++ useplaces} in (
	forall (i in index_set(usonly)) (table([useplaces[i], def[Alt(usonly[i])]], DomRel)) /\
	forall (i in index_set(defs)) (defplaces[i] = def[Alt(defs[i])]) /\
	forall (i in index_set(places), j in index_set(places) where i<j) (sel[m] -> places[i] = places[j]) /\
	forall (op in operationsCoveredByMatch[m]) (sel[m] -> oplace[op] = places[1]) /\
	forall (i in index_set(usonly)) (not sel[m] -> useplaces[i] = def[Alt(usonly[i])]) % prevent false symmetries
    )
  );

constraint
  forall (m in allMatches diff phiInstrMatches
          where card(spannedBlocksInMatch[m])>0) (
    let {array[int] of int: defs = [x | x in operandsDefinedByMatch[m]],
	 array[int] of int: usonly = [x | x in operandsUsedByMatch[m] diff operandsDefinedByMatch[m]],
	 array[index_set(usonly)] of var allBlocksInFunctionPlusNull: useplaces} in (
	forall (i in index_set(usonly)) (table([useplaces[i], def[Alt(usonly[i])]], DomRel)) /\
	forall (i in index_set(useplaces)) (sel[m] -> useplaces[i] = min(entryBlockOfMatch[m])) /\
	forall (o in operandsDefinedByMatch[m]) (sel[m] -> def[Alt(o)] in spannedBlocksInMatch[m]) /\
	forall (op in operationsCoveredByMatch[m]) (sel[m] -> oplace[op] = min(entryBlockOfMatch[m])) /\
	forall (i in index_set(usonly)) (not sel[m] -> useplaces[i] = def[Alt(usonly[i])]) % prevent false symmetries
    )
  );

constraint
  forall (m in phiInstrMatches) (
    let {array[int] of int: defs = [x | x in operandsDefinedByMatch[m]],
	 array[index_set(defs)] of var allBlocksInFunctionPlusNull: defplaces} in (
	forall (i in index_set(defs)) (defplaces[i] = def[Alt(defs[i])]) /\
	forall (i in index_set(defplaces), j in index_set(defplaces) where i<j) (sel[m] -> defplaces[i] = defplaces[j]) /\
	forall (op in operationsCoveredByMatch[m]) (sel[m] -> oplace[op] = defplaces[1])
    )
  );

% If a match representing a phi instruction is selected, then its data must be
% defined in the blocks indicated by the definition edges.
constraint
  forall (i in index_set_1of2(inputDefEdgesInMatch)) (
    let
    { int: m = inputDefEdgesInMatch[i, 1]
    , int: b = inputDefEdgesInMatch[i, 2]
    , int: o = inputDefEdgesInMatch[i, 3]
    } in
    sel[m] -> def[Alt(o)] = b
  );
constraint
  forall (i in index_set_1of2(outputDefEdgesInMatch)) (
    let
    { int: m = outputDefEdgesInMatch[i, 1]
    , int: b = outputDefEdgesInMatch[i, 2]
    , int: o = outputDefEdgesInMatch[i, 3]
    } in
    sel[m] -> def[Alt(o)] = b
  );

% Constrain locations of operands representing values for selected matches.
constraint
  forall (m in allMatches) (
    forall (o in allOperands) (
      let
      { set of int: locs = { matchValidDataLocs[i, 3]
                           | i in index_set_1of2(matchValidDataLocs)
                             where matchValidDataLocs[i, 1] = m
                             /\ matchValidDataLocs[i, 2] = o
                           }
      } in
      if card(locs) > 0
      then sel[m] -> loc[Alt(o)] in locs
      else true
      endif
    )
  );

% If a match representing a phi instruction is selected, then its data
% must be placed in the same location.
%
% [GHB] Used to be:
%   constraint
%     forall (m in phiInstrMatches) (
%       sel[m] -> all_equal([ loc[Alt(o)] | o in operandsExternalToMatch[m] ])
%     );
%
% [MC] Reformulation for solvers that can't reify all_equal.
constraint
  forall (m in phiInstrMatches, o in operandsExternalToMatch[m]) (
    let
    { int: o1 = min(operandsExternalToMatch[m])
    } in
    if o = o1 then true else sel[m] -> loc[Alt(o)] = loc[Alt(o1)] endif
  );

% If a match representing a null instruction is selected, then the data used by
% the match must be defined in the same block wherein the match is placed.
constraint
  forall (m in nullInstrMatches) (
    forall (o in operandsUsedByMatch[m]) (
      sel[m] -> def[Alt(o)] = place[m]
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
  forall ( m in allMatches
         , o in operandsExternalToMatch[m]
           where not (operandAlternatives[o] subset statesInFunction)
         )
  (
    sel[m] -> loc[Alt(o)] != locValueForNull
  );

% All data internal to a match must be located in the null location.
constraint
  forall ( m in allMatches
         , o in operandsInternalToMatch[m]
                where not (operandAlternatives[o] subset statesInFunction)
         )
  (
    sel[m] -> loc[Alt(o)] = locValueForNull
  );

% Constrain the loc value for all data that are states.
constraint
  forall (d in statesInFunction) (
    loc[d] = locValueForNull
  );

% Cost function.

array[int] of int: OCostFlat =
  [ if k=1 then o
    else if k=2 then m
    else if k=3 then p
    else let {int: prod = latencyOfMatch[m] * execFrequencyOfBlocksPlusNull[p],
              int: d = card(operationsCoveredByMatch[m]),
              int: q = prod div d,
              int: r = prod mod d} in
         if operationsCoveredByMatch[m][r+1]>o then q+1 else q endif
    endif
    endif
    endif
  | o in allOperationsInFunction
  , m in allMatches
  , p in if card(entryBlockOfMatch[m])>0 then entryBlockOfMatch[m]
            union
            {blockValueForNull}
         else allBlocksInFunctionPlusNull
         endif
  , k in 1..4
    where o in operationsCoveredByMatch[m]
  ];

int: OCostSize = card(index_set(OCostFlat)) div 4;

array[1..OCostSize, 1..4] of int: OCost =
  array2d(1..OCostSize, 1..4, OCostFlat);

set of int: OCostDom = {OCost[i,4] | i in 1..OCostSize};

constraint
  % cost = sum (m in allMatches)
  %        ( latencyOfMatch[m]
  %        * execFrequencyOfBlocksPlusNull[place[m]]
  %        ) /\
  (
    forall (o in allOperationsInFunction) (
      table([o, omatch[o], place[omatch[o]], oslice[o]], OCost)
    ) /\
    cost = sum(oslice)
  );

array[allOperationsInFunction] of var OCostDom: oslice;



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
    sel[m] -> loc[Alt(o)]
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

% Two matches can't both be selected, if they imply conflicting successor
% blocks.
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

% RULE 1A: If all matches for operation i, not a phi and no spanned blocks, use
%          datum u and define datum d, then the block defining u must dominate
%          the block defining d, and i must be placed in the block defining d.
constraint
  forall (i in allOperationsInFunction,
          d in allDataInFunction,
          u in allDataInFunction
	  where forall(m in omatchD[i])(not (m in phiInstrMatches) /\ card(spannedBlocksInMatch[m])=0)) (
    if forall (m in omatchD[i]) (
         exists (do in operandsDefinedByMatch[m]) (operandAlternatives[do] = {d}) /\
	 exists (uo in operandsUsedByMatch[m])    (operandAlternatives[uo] = {u})
    ) then (
      table([def[d], def[u]], DomRel) /\ oplace[i] = def[d]
    ) else true endif
  );

% RULE 1B: If all matches that define some data, not a phi and no spanned
%          blocks, use datum u and define datum d, then the block defining u
%          must dominate the block defining d.
constraint
  forall (M in dmatchDOnly,
          d in allDataInFunction,
          u in allDataInFunction
          where forall(m in M)(not (m in phiInstrMatches) /\ card(spannedBlocksInMatch[m])=0)) (
    if forall (m in M) (
         exists (do in operandsDefinedByMatch[m]) (operandAlternatives[do] = {d}) /\
         exists (uo in operandsUsedByMatch[m])    (operandAlternatives[uo] = {u})
    ) then (
      table([def[d], def[u]], DomRel)
    ) else true endif
  );

% RULE 1C: If all matches out of some oneof set, not a phi, have identical entry
%          blocks, use datum u, then the block defining u must dominate the
%          entry block,
constraint
  forall (M in omatchD++dmatchDOnly,
          u in allDataInFunction
	  where forall(m in M)(not (m in phiInstrMatches) /\ card(spannedBlocksInMatch[m])>0)) (
    let {int: Entry = min(entryBlockOfMatch[min(M)])} in
      if forall (m in M) (
	   min(entryBlockOfMatch[m]) = Entry /\
	   exists (uo in operandsUsedByMatch[m])    (operandAlternatives[uo] = {u})
      ) then (
	table([Entry, def[u]], DomRel)
      ) else true endif
  );

% RULE 1D: If all matches out of some oneof set have identical spanned blocks,
%          define datum d, then d must be placed in a spanned block.
constraint
  forall (M in omatchD++dmatchDOnly,
          d in allDataInFunction,
	  where forall(m in M)(card(spannedBlocksInMatch[m])>0)) (
    let {set of int: Spanned = spannedBlocksInMatch[min(M)]} in
      if forall (m in M) (
	   spannedBlocksInMatch[m] = Spanned /\
	   exists (do in operandsDefinedByMatch[m]) (operandAlternatives[do] = {d})
      ) then (
	def[d] in Spanned
      ) else true endif
  );

% RULE 1E: If all matches for operation i have identical entry blocks,
%          then i must be placed in the entry block.
constraint
  forall (i in allOperationsInFunction,
	  where forall(m in omatchD[i])(card(spannedBlocksInMatch[m])>0)) (
    let {int: Entry = min(entryBlockOfMatch[min(omatchD[i])])} in
      if forall (m in omatchD[i]) (
	   min(entryBlockOfMatch[m]) = Entry
      ) then (
	oplace[i] = Entry
      ) else true endif
  );

% RULE 1F: If all matches for operation i are phi and have the same def operand
%          then the def operand as well as i itself must all be placed in the
%          block mentioned in the output def edge.
constraint
  forall (i in allOperationsInFunction,
          d in allDataInFunction
	  where omatchD[i] subset phiInstrMatches) (
    let {set of int: BO = { outputDefEdgesInMatch[j,2]
                          | j in index_set_1of2(outputDefEdgesInMatch)
                            where outputDefEdgesInMatch[j,1] in omatchD[i]
			  },
         set of int: DO = { outputDefEdgesInMatch[j,3]
                          | j in index_set_1of2(outputDefEdgesInMatch)
                            where outputDefEdgesInMatch[j,1] in omatchD[i]
			  }} in
    if card(BO)=1 /\ forall(do in DO)(operandAlternatives[do] = {d})
    then
      def[d] = min(BO) /\ oplace[i] = min(BO)
    else true endif
  );

% RULE 3: If for given p,q, inBlockSucc contains [_, p, q] but does not contain
%         [_, p, q'] or [_, p', q] then succ[p] = q can only help, never hurt.
constraint
  let {array[allBlocksInFunction] of set of allBlocksInFunction: fwd = array1d(allBlocksInFunction,
                                            [ {s | i in index_set_1of2(inBlockSucc),
                                                   s in {inBlockSucc[i,3]}
                                                   where inBlockSucc[i,2] = b}
                                            | b in allBlocksInFunction
                                            ]),
       array[allBlocksInFunction] of set of allBlocksInFunction: bwd = array1d(allBlocksInFunction,
                                            [ {s | i in index_set_1of2(inBlockSucc),
                                                   s in {inBlockSucc[i,2]}
                                                   where inBlockSucc[i,3] = b}
                                            | b in allBlocksInFunction
                                            ])} in
    forall (p in allBlocksInFunction,
            q in allBlocksInFunction) (
      if fwd[p] = {q} /\ bwd[q] = {p} then
        succ[p] = q
      else true endif
    );



%==================
% SOLVE AND OUTPUT
%==================

solve
  % :: seq_search(
  %   [ bool_search([ sel[m]
  %                 | m in allMatchesBySize
  %                   where not ( m in copyInstrMatches
  %                                    union inactiveInstrMatches
  %                             )
  %                 ], input_order, indomain_max, complete)
  %   , int_search(alt, input_order, indomain_max, complete)
  %     ]
  % )
  :: int_search(oslice, largest, indomain_min, complete)
minimize cost;

array[allOperands] of int: matchOfO = [ m
		                      | o in allOperands, m in allMatches
		   		      where o in operandsDefinedByMatch[m] union operandsUsedByMatch[m]
				      ];

output [ "sel=", show(sel), "\n"
       , "alt=", show([if not fix(sel[matchOfO[o]]) then datumValueForNull else alt[o] endif | o in allOperands]), "\n"
       , "def=", show(def), "\n"
       , "loc=", show(loc), "\n"
       , "oplace=", show(oplace), "\n"
       , "place=", show([if not fix(sel[m]) then
                           blockValueForNull
                         else if card(operationsCoveredByMatch[m])=0 then
                           min(entryBlockOfMatch[m])
                         else
                           oplace[opOfM[m]]
                         endif endif | m in allMatches]), "\n"
       , "omatch=", show(omatch), "\n"
       , "dmatch=", show(dmatch), "\n"
       , "succ=", show(succ), "\n"
       , "block_value_for_null=", show(blockValueForNull), "\n"
       , "loc_value_for_null=", show(locValueForNull), "\n"
       , "alt_value_for_null=", show(datumValueForNull), "\n"
       , "cost=", show(execFrequencyGCD * cost), "\n"
       ];

% Local Variables:
% tab-width: 8
% End:
