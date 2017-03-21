%
%  Main authors:
%    Gabriel Hjort Blindell <ghb@kth.se>
%    Mats Carlsson <matsc@sics.se>
%
%  Contributing authors:
%    Roberto Castaneda Lozano <rcas@sics.se>
%
%  Copyright (c) 2012-2017, Gabriel Hjort Blindell <ghb@kth.se>
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
array[allBlocksInFunction] of set of int: domSetsOfBlocksInFunction;
array[allDataInFunction] of set of int: depsOfDataInFunction;
set of allOperationsInFunction: copiesInFunction;
set of allOperationsInFunction: controlOpsInFunction;
set of allDataInFunction: statesInFunction;
array[int, int] of int: stateDefEdgesForBlocksInFunction;
array[allBlocksInFunction] of int: execFrequencyOfBlockInFunction;
array[int] of set of int: interchangeableDataInFunction;
set of allDataInFunction: dataInFunctionUsedAtLeastOnce;

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
array[int, int] of int: validDataLocsInMatch;

array[int, int] of int: inputDefEdgesInMatch;
array[int, int] of int: outputDefEdgesInMatch;
array[allMatches] of int: codeSizeOfMatch;
array[allMatches] of int: latencyOfMatch;
set of allMatches: copyInstrMatches;
set of allMatches: killInstrMatches;
set of allMatches: nullInstrMatches;
set of allMatches: phiInstrMatches;
array[allMatches] of allMatches: allMatchesBySize;

int: costUpperBound;

% Arrays that encode constraints
array[int, int] of int: validDataLocsInFunction;
array[int, int] of int: validDataLocRangesInFunction;
array[int, int] of int: sameDataLocsInFunction;
array[int, int] of int: validDataLocRangesInMatch;
array[int, int] of int: sameDataLocsInMatch;
array[int, int] of int: fallThroughBlockOfMatch;
array[int] of set of int: illegalMatchCombinations;

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

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 1..numOperationsInFunction;
set of int: allDataInFunction = 1..numDataInFunction;
set of int: allBlocksInFunction = 1..numBlocksInFunction;
set of int: allMatches = 1..numMatches;
set of int: allOperands = 1..numOperands;
set of int: allLocValues = { x | x in canonicalDataLocs ++ [locValueForNull] };

% The 'domRelMatrix' matrix is a 2D matrix with 2 columns:
%    col 1: a block i
%        2: a block j that dominates i
% In other words, domRelMatrix has a row [i, j] if and only if j belongs to
% domSetsOfBlocksInFunction[i].
int: numDomMatrixRows =
  sum (b in allBlocksInFunction)
  ( card(domSetsOfBlocksInFunction[b]) );
array[1..numDomMatrixRows, 1..2] of allBlocksInFunction: domRelMatrix =
  array2d( 1..numDomMatrixRows
         , 1..2
         , [ if k = 1 then i else j endif | i in allBlocksInFunction
                                          , j in domSetsOfBlocksInFunction[i]
                                          , k in 1..2
           ]
         );

% The set of matches that can cover a particular operation.
array[allOperationsInFunction] of set of allMatches: matchsetOfOp =
  [ { m | m in allMatches where op in operationsCoveredByMatch[m] }
  | op in allOperationsInFunction
  ];

% The set of matches that can define a particular datum.
array[allDataInFunction] of set of allMatches: defsetOfDatum =
  [ { m | m in allMatches
        , o in operandsDefinedByMatch[m]
          where d in operandAlternatives[o]
    }
  | d in allDataInFunction
  ];

% The set of matches that can define a particular datum and does not cover any
% operations.
array[int] of set of allMatches: defsetOfDatumOnly =
  [ M | M in defsetOfDatum
        where forall (i in index_set(matchsetOfOp))
              ( matchsetOfOp[i] != M )
  ];

% Maps a match to an operation. This is only needed for figuring out in which
% block a selected match is to be placed.
array[allMatches] of int: opOfM =
  [ min({ o | o in allOperationsInFunction where m in matchsetOfOp[o] })
  | m in allMatches
  ];

% Maps an operand to a match. This is only needed for figuring out which
% operands have not been assigned a datum.
array[allOperands] of int: matchOfO =
  [ m
  | o in allOperands, m in allMatches
    where o in operandsDefinedByMatch[m] union operandsUsedByMatch[m]
  ];

% The total cost is computed as:
%
%   sum (m in allMatches)
%   ( latencyOfMatch[m] * execFrequencyOfBlocks[place[m]] )
%
% but this implementation yields poor propagation. To improve propagation, we
% use the cost per operation instead of cost per selected match.
%
% First, we split the cost incurred by selecting a given match over the
% operations that it covers. As the latency of a match times the exeqution
% frequency of a given block may not be evenly divisible by the number of
% operations covered, some operations may have a higher cost than others. The
% sum, however, is equal to the latency of the match times the execution
% frequency of the block in which the match is finally placed. This information
% is put in a matrix called 'costPerOpMatrix'.

% The 'costPerOpMatrix' is a 2D matrix with 4 columns:
%    col 1: an operation o
%        2: a match m that covers o
%        3: a block b in which m can be placed
%        4: the cost incurred by o if m is selected and placed in b
% For simplicity, we first create a list and then the matrix using the list.
array[int] of int: costPerOpList =
  [ if      k = 1 then o
    else if k = 2 then m
    else if k = 3 then b
    else let
         { int: prod = latencyOfMatch[m] * execFrequencyOfBlockInFunction[b]
         , int: d = card(operationsCoveredByMatch[m])
         , int: q = prod div d
         , int: r = prod mod d
         } in
         if operationsCoveredByMatch[m][r+1] > o then q+1 else q endif
    endif
    endif
    endif
  | o in allOperationsInFunction
  , m in allMatches
  , b in if card(entryBlockOfMatch[m]) > 0 then entryBlockOfMatch[m]
         else allBlocksInFunction
         endif
  , k in 1..4
    where o in operationsCoveredByMatch[m]
  ];
int: numCostPerOpMatrixRows = card(index_set(costPerOpList)) div 4;
array[1..numCostPerOpMatrixRows, 1..4] of int: costPerOpMatrix =
  array2d(1..numCostPerOpMatrixRows, 1..4, costPerOpList);

% A set with all the possible costs that can be incurred by any operation.
set of int: allOpCosts = { costPerOpMatrix[i, 4]
                         | i in 1..numCostPerOpMatrixRows
                         };

% The set of all non-phi instruction use operands.
set of int: nonPhiUseOperands =
  { p
  | m in allMatches diff phiInstrMatches
  , p in operandsUsedByMatch[m]
  };

% The set of operands which are either used or defined by kill instructions.
set of int: killOperands =
  array_union( [ operandsDefinedByMatch[m] | m in killInstrMatches ]
               ++
               [ operandsUsedByMatch[m] | m in killInstrMatches ]
             );




%===========
% VARIABLES
%===========

% Match selection.
array[allMatches] of var bool: sel;

% Blocks wherein the data are placed.
array[allDataInFunction] of var allBlocksInFunction: dplace;

% The block in which a particular operation is placed.
array[allOperationsInFunction] of var allBlocksInFunction: oplace;

% Data locations.
array[allDataInFunction] of var allLocValues: loc;

% Data selected for the operands.
array[allOperands] of var allDataInFunction: alt;

% Block ordering (succ[b] is the block appearing immeditely after block b in the
% generated code).
array[allBlocksInFunction] of var allBlocksInFunction: succ;

% Cost.
var int: totalcost;
array[allOperationsInFunction] of var allOpCosts: opcosts;



%================
% DUAL VARIABLES
%================

% The match that covers a particular operation.
array[allOperationsInFunction] of var allMatches: omatch;

% The match that defines a particular datum.
array[allDataInFunction] of var allMatches: dmatch;

% For selected, non-null, non-phi instruction use operands:
%   block where a given operand is used.
% For non-selected, non-null, non-phi instruction use operands:
%   block where a given operand is defined.
% For other operands:
%   1.
array[allOperands] of var allBlocksInFunction: uplace;



%====================
% GLOBAL CONSTRAINTS
%====================

include "globals.mzn";



%===========
% FUNCTIONS
%===========

% Bypasses the alt[.] variable if the given operand only has one alternative.
% Valid under the assumption that its operation was selected.
var allDataInFunction: Alt(allOperands: o) =
  let { allDataInFunction: d = min(operandAlternatives[o]) }
  in if card(operandAlternatives[o]) = 1 then d else alt[o] endif;


% True if operand o for sure cannot be in the null location.
test oLocCantBeNull(allOperands: o) =
  exists(i in index_set_1of2(validDataLocRangesInMatch))(
    validDataLocRangesInMatch[i,2] = o /\
    validDataLocRangesInMatch[i,4] < locValueForNull
  );

% True if datum d for sure cannot be in the null location.
test dLocCantBeNull(allDataInFunction: d) =
  forall ( m in defsetOfDatum[d] )
         ( not (m in killInstrMatches) /\
           exists ( use2 in operandsDefinedByMatch[m] intersect
                            operandsExternalToMatch[m]
                  )
           ( operandAlternatives[use2] = {d} )
         );

%==================
% BASE CONSTRAINTS
%==================

% Constrain locations of data representing values.
constraint
  forall (d in allDataInFunction)
  ( let { set of int: locs = { validDataLocsInFunction[i, 2]
                             | i in index_set_1of2(validDataLocsInFunction)
                               where validDataLocsInFunction[i, 1] = d
                             }
        }
    in if card(locs) > 0 then loc[d] in locs else true endif
  );

% Constrain locations of data that must be within a specific range.
constraint
  forall (i in index_set_1of2(validDataLocRangesInFunction))
  ( let { int: d = validDataLocRangesInFunction[i, 1]
        , int: l = validDataLocRangesInFunction[i, 2]
        , int: u = validDataLocRangesInFunction[i, 3]
        }
    in loc[d] in l..u
  );


% Constrain locations of data which must be assigned the same location.
constraint
  forall (i in index_set_1of2(sameDataLocsInFunction)) (
    let { int: o1 = sameDataLocsInFunction[i, 1]
        , int: o2 = sameDataLocsInFunction[i, 2]
        }
    in loc[Alt(o1)] = loc[Alt(o2)]
  );

% Constrain alternatives of operands.
constraint
  forall (o in allOperands)
  ( alt[o] in operandAlternatives[o] );

% All operands external to a match, and which are not states, must not be
% located in the null location.
% [MC 1] Exclude kill matches from this constraint.
constraint
  forall ( m in allMatches diff killInstrMatches
         , o in operandsExternalToMatch[m]
           where not (operandAlternatives[o] subset statesInFunction)
              /\ not oLocCantBeNull(o)
         )
  ( if forall(d in operandAlternatives[o])(dLocCantBeNull(d)) then true else
      sel[m] -> loc[Alt(o)] != locValueForNull
    endif
  );

% All operands internal to a match must be located in the null location.
constraint
  forall ( m in allMatches
         , o in operandsInternalToMatch[m]
           where not (operandAlternatives[o] subset statesInFunction)
         )
  ( sel[m] -> loc[Alt(o)] = locValueForNull );

% If a match representing a phi instruction is selected, then its operands
% must be placed in the same location.
constraint
  forall ( m in phiInstrMatches
         , o1 in operandsExternalToMatch[m]
         )
  ( let { int: o2 = min(operandsExternalToMatch[m]) }
    in if o1 != o2 then sel[m] -> loc[Alt(o1)] = loc[Alt(o2)] else true endif
  );

% Constrain locations of operands representing values for selected matches.
constraint
  forall ( m in allMatches
         , o in operandsDefinedByMatch[m] union operandsUsedByMatch[m]
         )
  ( let { set of int: locs = { validDataLocsInMatch[i, 3]
                             | i in index_set_1of2(validDataLocsInMatch)
                               where validDataLocsInMatch[i, 1] = m
                                     /\
                                     validDataLocsInMatch[i, 2] = o
                             }
        }
    in if card(locs) > 0
       then sel[m] -> loc[Alt(o)] in locs
       else true
       endif
  );

% Constrain locations of operands that must be within a specific range.
constraint
  forall (i in index_set_1of2(validDataLocRangesInMatch)) (
    let { int: m = validDataLocRangesInMatch[i, 1]
        , int: o = validDataLocRangesInMatch[i, 2]
        , int: l = validDataLocRangesInMatch[i, 3]
        , int: u = validDataLocRangesInMatch[i, 4]
        }
    in sel[m] -> loc[Alt(o)] in l..u
  );

% For selected matches that require two or more of its operands to have the same
% location, enforce them to be the same.
constraint
  forall (i in index_set_1of2(sameDataLocsInMatch)) (
    let { int: m = sameDataLocsInMatch[i, 1]
        , int: o1 = sameDataLocsInMatch[i, 2]
        , int: o2 = sameDataLocsInMatch[i, 3]
        }
    in sel[m] -> loc[Alt(o1)] = loc[Alt(o2)]
  );

% If a match representing a phi instruction is selected, then its data must be
% defined in the blocks indicated by the definition edges.
constraint
  forall (i in index_set_1of2(inputDefEdgesInMatch)) (
    let { int: m = inputDefEdgesInMatch[i, 1]
        , int: b = inputDefEdgesInMatch[i, 2]
        , int: o = inputDefEdgesInMatch[i, 3]
        }
    in sel[m] -> dplace[Alt(o)] = b
  );

constraint
  forall (i in index_set_1of2(outputDefEdgesInMatch)) (
    let { int: m = outputDefEdgesInMatch[i, 1]
        , int: b = outputDefEdgesInMatch[i, 2]
        , int: o = outputDefEdgesInMatch[i, 3]
        }
    in sel[m] -> dplace[Alt(o)] = b
  );

% A state datum with a definition edge with a block must be defined in the block
% of that edge.
constraint
  forall (i in index_set_1of2(stateDefEdgesForBlocksInFunction)) (
    let { int: b = stateDefEdgesForBlocksInFunction[i, 1]
        , int: d = stateDefEdgesForBlocksInFunction[i, 2]
        }
    in dplace[d] = b
  );

% If a match is selected, then all operations covered by that match must be
% placed in the same block.
constraint
  forall ( m in allMatches
         , o in operationsCoveredByMatch[m]
           where o != opOfM[m]
         )
  ( sel[m] -> oplace[opOfM[m]] = oplace[o] );

% If a selected match m has an entry block b, then all operations covered by m
% must be placed in b.
%
% If a match has no entry block, then this set will be empty and hence there
% will be no such constraint. It is assumed that there will be at most one
% entry.
constraint
  forall ( m in allMatches
         , b in entryBlockOfMatch[m]
         , o in operationsCoveredByMatch[m]
         )
  ( sel[m] -> oplace[o] = b );

% Data defined by a selected match m must be defined either in one of the blocks
% spanned by m, or the block wherein the operations covered by m are placed.
constraint
  forall ( m in allMatches
         , o in operandsDefinedByMatch[m]
         )
  ( sel[m] -> if card(spannedBlocksInMatch[m]) > 0
              then dplace[Alt(o)] in spannedBlocksInMatch[m]
              else forall (op in operationsCoveredByMatch[m])
                   ( dplace[Alt(o)] = oplace[op] )
              endif
  );

% No operations may be placed in a block which is consumed by some selected
% match.
constraint
  forall ( o in allOperationsInFunction
         , m in allMatches
         , b in consumedBlocksInMatch[m]
         )
  ( sel[m] -> oplace[o] != b );

% For each selected match m that apply fall-through, enforce that the
% fall-through block of m is the immediate successor of the entry block of m.
constraint
  forall (i in index_set_1of2(fallThroughBlockOfMatch))
  ( let { int: m = fallThroughBlockOfMatch[i, 1]
        , int: fall_b = fallThroughBlockOfMatch[i, 2]
        }
    in sel[m] -> falls_through(m, fall_b)
  );

predicate falls_through(allMatches: m, allBlocksInFunction: fall_b) =
  let { int: entry_b = min(entryBlockOfMatch[m])
      , var int: succ_b = succ[entry_b]
      }
  in  succ_b = fall_b \/
      ( succ[succ_b] = fall_b /\
	forall (o in allOperationsInFunction)
	( oplace[o] != succ_b \/
	  omatch[o] in nullInstrMatches
	)
      );

% Enforce that, for each operation o, exactly one match must be selected such
% that o is covered.
%
% THIS HAS BEEN REPLACED WITH DUAL VARIABLE CONSTRAINT.

% Enforce that every datum is defined in a block such that the block dominates
% all blocks wherein the datum is used. This constraint shall not be applied to
% the generic phi patterns, nor to null instructions.
%
% This used to be enforced by the following constraint:
%   constraint
%     forall ( m in allMatches
%            , o in operandsUsedByMatch[m]
%              where not (m in phiInstrMatches)
%            )
%     ( dplace[alt[o]] in domSetsOfBlocksInFunction[place[m]] );
% but it has been reformulated using table constraints to avoid the use of set
% variables.
%
% [MC] Assumes that matches which use some data cover at least one operation.
% [GHB] This assumption should always hold.
constraint
  forall (p in nonPhiUseOperands)
  ( table([uplace[p], dplace[Alt(p)]], domRelMatrix) );

constraint
  forall (p in allOperands diff nonPhiUseOperands)
  ( uplace[p] = 1 );

constraint
  forall ( m in allMatches diff phiInstrMatches
         , p in operandsUsedByMatch[m]
         )
  ( not sel[m] -> uplace[p] = dplace[Alt(p)] );

constraint
  forall ( m in allMatches diff phiInstrMatches
         , o in operandsUsedByMatch[m]
         , c in operationsCoveredByMatch[m]
         )
  ( sel[m] -> oplace[c] = uplace[o] );

% If a datum d is used by at least one operand in at least one selected match,
% then d must not be defined by any kill match.  Otherwise, d must be
% defined by an kill match.
% constraint
%   forall ( mi in killInstrMatches
%          , oi in operandsDefinedByMatch[mi]
%          , d in operandAlternatives[oi]
%          )
%   ( exists ( m in allMatches
%            , o in operandsUsedByMatch[m]
%              where d in operandAlternatives[o]
%            )
%     ( sel[m] /\ alt[o] = d )
%     <->
%     not sel[mi]
%   );
% [MC 1]
% If and only if a datum d is used by at least one operand in at least
% one selected match, then d must not be defined by any kill
% match.  If and only if d is used by at least one external operand in
% at least one selected match, then d must not placed in the null
% location.
constraint
  forall ( mi in killInstrMatches
         , oi in operandsDefinedByMatch[mi]
         , d in operandAlternatives[oi]
         )
  ( let { var bool: ui,
          var bool: ue } in
    ( ui <-> exists ( m in allMatches
                    , o in operandsUsedByMatch[m] intersect
                           operandsInternalToMatch[m]
                    where d in operandAlternatives[o]
                    )
             ( sel[m] /\ alt[o] = d ) ) /\
    ( ue <-> exists ( m in allMatches
                    , o in operandsUsedByMatch[m] intersect
                           operandsExternalToMatch[m]
                    where d in operandAlternatives[o]
                    )
             ( sel[m] /\ alt[o] = d ) ) /\
    ( sel[mi] <-> not ui /\ not ue ) /\
    ( loc[d] != locValueForNull <-> ue )
  );

% Ensure that succ forms a circuit (thus resulting in an ordering of blocks).
constraint
  if card(allBlocksInFunction) > 1
  then circuit(succ) :: domain
  else true
  endif;

% Forbid matches in an illegal combination from all being selected.
constraint
  forall (c in illegalMatchCombinations)
  ( sum (m in c) (bool2int(sel[m])) < card(c) );

% Constrain the cost that can be incurred by each operation.
constraint
  forall (op in allOperationsInFunction)
  ( table([op, omatch[op], oplace[op], opcosts[op]], costPerOpMatrix) );

% The total cost is the sum of the costs incurred by all operations.
constraint
  totalcost = sum(opcosts);

% Constraint the upper bound of the cost (retrieved from LLVM).
constraint
  if costUpperBound > 0
  then totalcost < costUpperBound
  else true
  endif;



%===========================
% DUAL VARIABLE CONSTRAINTS
%===========================

% For each operation o, exactly one match must be selected such that o is
% covered.
%
% This replaces the constraint that, for each operation o, exactly one match
% must be selected such that o is covered:
%
%   constraint
%     forall (o in allOperationsInFunction)
%     ( let { set of int: mset = { m | m in allMatches
%                                      where o in operationsCoveredByMatch[m]
%                                }
%           }
%       in sum (m in mset) (bool2int(sel[m])) = 1
%     );
constraint
  forall (o in allOperationsInFunction)
  ( omatch[o] in matchsetOfOp[o]
    /\
    forall (m in matchsetOfOp[o])
    ( omatch[o] = m <-> sel[m] )
  );

% For each datum d, exactly one match must be selected such that d is defined.
%
% This replaces the constraint that that, for each datum d, exactly one match
% must be selected such that d is defined:
%
%   constraint
%     forall (d in allDataInFunction)
%     ( let { set of int: mset = { m | m in allMatches
%                                    , o in operandsDefinedByMatch[m]
%                                      where d in operandAlternatives[o]
%                                }
%           }
%       in sum (m in mset) (bool2int(sel[m])) = 1
%     );
%
% This is an implied constraint, but it also enforces that the patterns for
% defining the function input and constants are selected. Such patterns do not
% cover any operations, they are not entailed in the above constraint for
% exactly covering each operation.
constraint
  forall (d in allDataInFunction)
  ( dmatch[d] in defsetOfDatum[d]
    /\
    forall (m in defsetOfDatum[d])
    ( dmatch[d] = m <-> sel[m] )
  );



%=======================
% DOMINANCE CONSTRAINTS
%=======================

% Constrain the loc value for all data that are states.
constraint
  forall (d in statesInFunction)
  ( loc[d] = locValueForNull );

% Fix operand value if match was not selected.
constraint
  forall ( m in allMatches
         , o in operandsDefinedByMatch[m] union operandsUsedByMatch[m]
           where card(operandAlternatives[o]) > 1
         )
  ( not sel[m] -> alt[o] = min(operandAlternatives[o]) );

% Break symmetries introduced by interchangeable data.
%
% [GHB] This could remove potential optimal solutions in situations where data
% is interchangeable WITHIN a group of matches, but not BETWEEN groups of
% matches. Have yet to see such an occurrance, though.
constraint
  forall ( chain in interchangeableDataInFunction )
  ( let { set of int: ixset = { y | y in nonPhiUseOperands
                                    where operandAlternatives[y] = chain
                              }
        }
    in if card(ixset) = 0
       then true
       else value_precede_chain(chain, [alt[y] | y in ixset])
       endif
  );

% A consequence of symmetry breaking.
constraint
  forall ( chain in interchangeableDataInFunction
           where forall (d in chain)
                 ( defsetOfDatum[d] subset copyInstrMatches )
         )
  ( let { array[int] of int: kills =
            [ min(defsetOfDatum[d] intersect killInstrMatches) | d in chain ]
        }
    in increasing (m in kills)
       ( sel[m] )
  );



%=====================
% IMPLIED CONSTRAINTS
%=====================

% TODO: add constraint that makes use of depsOfDataInFunction

% Two matches cannot be selected with same fall-thru blocks but different entry
% blocks. Two matches cannot be selected with different fall-thru blocks but
% same entry blocks.
constraint
  forall ( i in index_set_1of2(fallThroughBlockOfMatch)
         , j in index_set_1of2(fallThroughBlockOfMatch)
           where i < j
         )
  ( let { int:       m1 = fallThroughBlockOfMatch[i, 1]
        , int:  fall_b1 = fallThroughBlockOfMatch[i, 2]
        , int: entry_b1 = min(entryBlockOfMatch[m1])
        , int:       m2 = fallThroughBlockOfMatch[j, 1]
        , int:  fall_b2 = fallThroughBlockOfMatch[j, 2]
        , int: entry_b2 = min(entryBlockOfMatch[m2])
        }
    in if (fall_b1 = fall_b2) xor (entry_b1 = entry_b2)
       then not sel[m1] \/ not sel[m2]
       else true
       endif
  );

% Two matches cannot both be selected if the first match implies that two
% locations are equal, and the second match implies that the intersection of
% their domains is empty.
%
% [GHB] Disabled this constraint as it explodes flattening time.
%
%constraint
%  forall (i in index_set_1of2(sameDataLocsInMatch))
%  ( let { int: m1 = sameDataLocsInMatch[i, 1]
%        , int: o1 = sameDataLocsInMatch[i, 2]
%        , int: o2 = sameDataLocsInMatch[i, 3]
%        }
%    in forall ( j in index_set_1of2(validDataLocRangesInMatch)
%              , k in index_set_1of2(validDataLocRangesInMatch)
%                where validDataLocRangesInMatch[j, 2] = o1
%                      /\
%                      validDataLocRangesInMatch[k, 1] =
%                      validDataLocRangesInMatch[j, 1]
%                      /\
%                      validDataLocRangesInMatch[k, 2] = o2
%              )
%       ( let { int: m2 = validDataLocRangesInMatch[j, 1] }
%         in card(
%              validDataLocRangesInMatch[j, 3]..validDataLocRangesInMatch[j, 4]
%              intersect
%              validDataLocRangesInMatch[k, 3]..validDataLocRangesInMatch[k, 4]
%            ) = 0
%            ->
%            (not sel[m1] \/ not sel[m2])
%
%       )
%  );

% If all matches for operation o are not phi instruction, do not span any
% blocks, use datum u, and define datum d, then the block defining u must
% dominate the block defining d, and o must be placed in the block defining d.
constraint
  forall ( o in allOperationsInFunction
           where forall (m in matchsetOfOp[o])
                 ( not (m in phiInstrMatches) /\
                   card(spannedBlocksInMatch[m]) = 0
                 )
         )
  ( let { set of allMatches: M = matchsetOfOp[o]
        , set of allDataInFunction: D =
            { d | do in operandsDefinedByMatch[min(M)]
                , d in operandAlternatives[do]
            }
        , set of allDataInFunction: U = { u | uo in operandsUsedByMatch[min(M)]
                                            , u in operandAlternatives[uo]
                                        }
        }
    in forall (d in D, u in U)
       ( if forall (m in M)
            ( exists (do in operandsDefinedByMatch[m])
              ( operandAlternatives[do] = {d} )
              /\
              exists (uo in operandsUsedByMatch[m])
              ( operandAlternatives[uo] = {u} )
            )
         then table([dplace[d], dplace[u]], domRelMatrix) /\
              oplace[o] = dplace[d]
         else true
         endif
       )
  );

% If all matches that define some data, does not contain any phi instructions
% nor instructions that span any blocks, use datum u and define datum d, then
% the block defining u must dominate the block defining d.
constraint
  forall ( M in defsetOfDatumOnly
           where forall (m in M)
                 ( not (m in phiInstrMatches)
                   /\
                   card(spannedBlocksInMatch[m]) = 0
                 )
         )
  ( let { set of allDataInFunction: D =
            { d | do in operandsDefinedByMatch[min(M)]
                , d in operandAlternatives[do]
            }
        , set of allDataInFunction: U = { u | uo in operandsUsedByMatch[min(M)]
                                            , u in operandAlternatives[uo]
                                        }
        }
    in forall (d in D, u in U)
       ( if forall (m in M)
            ( exists (do in operandsDefinedByMatch[m])
              ( operandAlternatives[do] = {d} )
              /\
              exists (uo in operandsUsedByMatch[m])
              ( operandAlternatives[uo] = {u} )
            )
         then table([dplace[d], dplace[u]], domRelMatrix)
         else true
         endif
       )
  );

% If all matches in the matchset that either covers a particular operation or
% defines a datum, does not contain any phi instructions, have identical entry
% blocks, and use datum u, then the block defining u must dominate the entry
% block.
constraint
  forall ( M in matchsetOfOp ++ defsetOfDatumOnly
         , u in allDataInFunction
           where forall (m in M)
                 ( not (m in phiInstrMatches)
                   /\
                   card(spannedBlocksInMatch[m])>0
                 )
         )
  ( let { int: Entry = min(entryBlockOfMatch[min(M)]) }
    in if forall (m in M)
          ( min(entryBlockOfMatch[m]) = Entry /\
            exists (uo in operandsUsedByMatch[m])
            ( operandAlternatives[uo] = {u} )
          )
       then table([Entry, dplace[u]], domRelMatrix)
       else true
       endif
  );

% If all matches in the matchset that either covers a particular operation or
% defines a datum, have identical spanned blocks, and define datum d, then d
% must be placed in a spanned block.
constraint
  forall ( M in matchsetOfOp ++ defsetOfDatumOnly
         , d in allDataInFunction,
           where forall (m in M)
                 ( card(spannedBlocksInMatch[m]) > 0 )
         )
  ( let { set of int: Spanned = spannedBlocksInMatch[min(M)] }
    in if forall (m in M)
          ( spannedBlocksInMatch[m] = Spanned /\
            exists (do in operandsDefinedByMatch[m])
            ( operandAlternatives[do] = {d} )
          )
       then dplace[d] in Spanned
       else true
       endif
  );

% If all matches for an operation o have identical entry blocks, then o must be
% placed in the entry block.
constraint
  forall ( o in allOperationsInFunction
           where forall (m in matchsetOfOp[o])
                 ( card(spannedBlocksInMatch[m]) > 0 )
         )
  ( let { int: Entry = min(entryBlockOfMatch[min(matchsetOfOp[o])]) }
    in if forall (m in matchsetOfOp[o])
          ( min(entryBlockOfMatch[m]) = Entry )
       then oplace[o] = Entry
       else true
       endif
  );

% If all matches for an operation o are all phi instructions and have the same
% def operand, then the def operand as well as o itself must all be placed in
% the block mentioned in the output def edge.
constraint
  forall ( o in allOperationsInFunction
         , d in allDataInFunction
           where matchsetOfOp[o] subset phiInstrMatches
         )
  ( let { set of int: BO = { outputDefEdgesInMatch[j, 2]
                           | j in index_set_1of2(outputDefEdgesInMatch)
                             where outputDefEdgesInMatch[j, 1] in
                                   matchsetOfOp[o]
                           }
        , set of int: DO = { outputDefEdgesInMatch[j, 3]
                           | j in index_set_1of2(outputDefEdgesInMatch)
                             where outputDefEdgesInMatch[j, 1] in
                                   matchsetOfOp[o]
                           }
        }
    in if card(BO) = 1 /\ forall (do in DO)
                          ( operandAlternatives[do] = {d} )
       then dplace[d] = min(BO) /\ oplace[o] = min(BO)
       else true
       endif
  );

% If for any two given blocks p and q, and fallThroughBlockOfMatch contains
% [_, p, q] but does not contain [_, p, q'] or [_, p', q], then succ[p] = q can
% only help, never hurt.
constraint
  let { array[allBlocksInFunction] of set of allBlocksInFunction: fwd =
          array1d( allBlocksInFunction
                 , [ { s | i in index_set_1of2(fallThroughBlockOfMatch)
                         , m in {fallThroughBlockOfMatch[i, 1]}
                         , s in {fallThroughBlockOfMatch[i, 2]}
                           where entryBlockOfMatch[m] = {b}
                     }
                   | b in allBlocksInFunction
                   ]
                 )
      , array[allBlocksInFunction] of set of allBlocksInFunction: bwd =
          array1d( allBlocksInFunction
                 , [ { s | i in index_set_1of2(fallThroughBlockOfMatch)
                         , m in {fallThroughBlockOfMatch[i, 1]}
                         , s in entryBlockOfMatch[m]
                           where fallThroughBlockOfMatch[i, 2] = b
                     }
                   | b in allBlocksInFunction
                   ]
                 )
      }
  in forall ( p in allBlocksInFunction
            , q in allBlocksInFunction
            )
     ( if fwd[p] = {q} /\ bwd[q] = {p}
       then succ[p] = q
       else true
       endif
     );

% For non-phi matches, no spanned blocks:
% if selected, blocks of outputs and blocks of use of inputs must be all equal.
constraint
  forall ( m in allMatches diff phiInstrMatches
           where card(spannedBlocksInMatch[m]) = 0
         )
  ( let { array[int] of int: defs = [x | x in operandsDefinedByMatch[m]]
        , array[int] of int: usonly = [ x | x in operandsUsedByMatch[m]
                                            diff operandsDefinedByMatch[m]
                                      ]
        }
    in forall ( i in index_set(usonly)
              , j in index_set(usonly)
                where i < j
              )
       ( sel[m] -> uplace[usonly[i]] = uplace[usonly[j]] )
       /\
       forall ( i in index_set(defs)
              , j in index_set(defs)
                where i < j
              )
       ( sel[m] -> dplace[Alt(defs[i])] = dplace[Alt(defs[j])] )
       /\
       forall ( i in index_set(usonly)
              , j in index_set(defs)
              )
       ( sel[m] -> uplace[usonly[i]] = dplace[Alt(defs[j])] )
  );

% For non-phi matches, spanned blocks:
% if selected, blocks of use of inputs must be all equal.
constraint
  forall ( m in allMatches diff phiInstrMatches
           where card(spannedBlocksInMatch[m]) > 0
         )
  ( let { array[int] of int: use = [x | x in operandsUsedByMatch[m]] }
    in forall ( i in index_set(use)
              , j in index_set(use)
                where i < j
              )
       ( sel[m] -> uplace[use[i]] = uplace[use[j]] )
  );

% [MC 2]
%
% If all matches in the matchset that either covers a particular operation or
% defines a datum, and use some datum d in an external operand, then d
% cannot be placed in the null location.
constraint
  forall ( M in matchsetOfOp ++ defsetOfDatumOnly )
  ( let { set of int: uses1 = operandsUsedByMatch[min(M)] intersect
                              operandsExternalToMatch[min(M)]
        }
    in forall (use1 in uses1)
       ( let { set of int: data1 = operandAlternatives[use1] }
         in if forall ( m in M where m > min(M) )
               ( exists ( use2 in operandsUsedByMatch[m] intersect
                                  operandsExternalToMatch[m]
                        )
                 ( operandAlternatives[use2] = data1 )
               ) /\
               forall (d in data1)(not dLocCantBeNull(d))
            then exists (d in data1)
                 ( loc[d] != locValueForNull )
            else true
            endif
       )
  );

% [MC 3]
%
% If all matches in the matchset that defines a non-state datum d, are active,
% and define it in an external operand, then d must not be placed in the null
% location.
%
% [MC 3] does not dominate [MC 2], nor vice versa.
constraint
  forall (d in allDataInFunction diff statesInFunction )
  ( if dLocCantBeNull(d)
    then loc[d] != locValueForNull
    else true
    endif
  );

% [MC 4]
%
% If an external operand does not take its min value, then its match must be
% selected and hence the datum cannot be in the null location.
constraint
  forall ( m in allMatches
         , o in operandsUsedByMatch[m] intersect operandsExternalToMatch[m]
                where card(operandAlternatives[o]) > 1
                   /\ not oLocCantBeNull(o)
         )
  ( alt[o] != min(operandAlternatives[o]) -> loc[alt[o]] != locValueForNull );

% [MC 5]
%
% Constrain the location of d to be where its definers can put it. Also, disable
% users that would access it elsewhere.
constraint
  forall (d in allDataInFunction diff statesInFunction)
  ( let { array[int] of int: O =
            [o | m in defsetOfDatum[d] diff killInstrMatches
               , o in operandsDefinedByMatch[m]
                 where d in operandAlternatives[o]
            ]
        , array[int] of int: X =
            [ i | i in index_set_1of2(validDataLocRangesInMatch)
                  where validDataLocRangesInMatch[i, 2] in {o | o in O}
            ]
        }
    in if length(O) = length(X)
       then let { set of int: D =
                    { addr | addr in canonicalDataLocs
                           , i in X
                             where addr in validDataLocRangesInMatch[i, 3]
                                           ..
                                           validDataLocRangesInMatch[i, 4]
                    }
                }
            in if card(D) < card(canonicalDataLocs)
               then knock_out_users(d, D union {locValueForNull}) /\
                    loc[d] in D union {locValueForNull}
               else true
               endif
       else true
       endif
  );

predicate knock_out_users(allDataInFunction: d, set of allLocValues: D) =
  let { array[int] of int: O =
          [ o | o in array_union(operandsUsedByMatch) diff killOperands
                     where d in operandAlternatives[o]
          ]
      , array[int] of int: I =
          [ i | i in index_set_1of2(validDataLocRangesInMatch)
                     where validDataLocRangesInMatch[i,2] in {o | o in O} /\
                           card( ( validDataLocRangesInMatch[i, 3]
                                   ..
                                   validDataLocRangesInMatch[i, 4]
                                 )
                                 intersect
                                 D
                               ) = 0
          ]
      }
  in forall (i in I)
     ( not sel[validDataLocRangesInMatch[i, 1]] );

% [MC 6]
%
% Constrain the location of d to be where its users can access it. Also, disable
% definers that would put it elsewhere. Valid for such d that are used at least
% once.
constraint
  forall (d in dataInFunctionUsedAtLeastOnce)
  ( let { array[int] of int: O =
            [ o | o in array_union(operandsUsedByMatch) diff killOperands
                  where d in operandAlternatives[o]
            ]
        , array[int] of int: X =
            [ i | i in index_set_1of2(validDataLocRangesInMatch)
                  where validDataLocRangesInMatch[i,2] in {o | o in O}
            ]
        }
    in if length(O) = length(X)
       then let { set of int: D =
                    { addr | addr in canonicalDataLocs
                           , i in X
                             where addr in validDataLocRangesInMatch[i, 3]
                                           ..
                                           validDataLocRangesInMatch[i, 4]
                    }
                }
            in if card(D) < card(canonicalDataLocs)
               then knock_out_definers(d, D union {locValueForNull}) /\
                    loc[d] in D union {locValueForNull}
               else true
               endif
       else true
       endif
  );

predicate knock_out_definers(allDataInFunction: d, set of allLocValues: D) =
  forall (m in defsetOfDatum[d])
  ( let { array[int] of int: O =
            [ o | o in operandsDefinedByMatch[m]
                  where d in operandAlternatives[o]
            ]
        }
    in if exists (i in index_set_1of2(validDataLocRangesInMatch))
          ( validDataLocRangesInMatch[i,2] in {o | o in O} /\
            card( ( validDataLocRangesInMatch[i, 3]
                    ..
                    validDataLocRangesInMatch[i, 4]
                  )
                  intersect
                  D
                ) = 0
          )
       then not sel[m]
       else true
       endif
  );



%==================
% SOLVE AND OUTPUT
%==================

solve
  :: % Try the smallest cost for the operation that incurs the largest cost.
     int_search(opcosts, largest, indomain_min, complete)
minimize totalcost;

% oplace, omatch, dmatch are handy for debugging
output [ "sel=", show(sel), "\n"
       , "alt=", show([ if not fix(sel[matchOfO[o]])
                        then datumValueForNull
                        else alt[o] endif
                      | o in allOperands
                      ]), "\n"
       , "dplace=", show(dplace), "\n"
       , "loc=", show(loc), "\n"
       , "place=", show([ if not fix(sel[m])
                          then blockValueForNull
                          else if card(operationsCoveredByMatch[m]) = 0
                               then min(entryBlockOfMatch[m])
                               else oplace[opOfM[m]]
                               endif
                          endif
                        | m in allMatches
                        ]), "\n"
       , "oplace=", show(oplace), "\n"
       , "omatch=", show(omatch), "\n"
       , "dmatch=", show(dmatch), "\n"
       , "succ=", show(succ), "\n"
       , "block_value_for_null=", show(blockValueForNull), "\n"
       , "loc_value_for_null=", show(locValueForNull), "\n"
       , "alt_value_for_null=", show(datumValueForNull), "\n"
       , "cost=", show(execFrequencyGCD * totalcost), "\n"
       ];



% Local Variables:
% tab-width: 8
% End:
