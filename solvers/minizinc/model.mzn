% Copyright (c) 2014, Gabriel Hjort Blindell <ghb@kth.se>
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

% Function data
int: numOpsInFunction;
int: numDataInFunction;
int: numStatesInFunction;
int: numLabelsInFunction;
int: rootLabelOfFunction;
array[allLabelsInFunction] of set of int: domsetOfLabelInFunction;
array[int] of allOperationsInFunction: essentialOperationsInFunction;
array[allLabelsInFunction] of int: execFrequencyOfLabelInFunction;
array[allDataInFunction] of set of allLabelsInFunction:
  labelsWhereDataInFunctionMustBeDefined;
array[allStatesInFunction] of set of allLabelsInFunction:
  labelsWhereStatesInFunctionMustBeDefined;

% Target machine data
int: numRegisters;

% Match data
int: numMatches;
array[allMatches] of set of int: opsCoveredByMatch;
array[allMatches] of set of int: dataDefinedByMatch;
array[allMatches] of set of int: dataUsedInMatch;
array[allMatches] of set of int: statesDefinedByMatch;
array[allMatches] of set of int: statesUsedInMatch;
array[allMatches] of set of int: rootLabelOfMatch;
array[allMatches] of set of int: nonRootLabelsInMatch;
array[allMatches] of int: codeSizeOfMatch;
array[allMatches] of int: latencyOfMatch;
array[allMatches] of bool: applyDefDomUseConstraintForMatch;
array[allMatches, allLabelsInFunction] of int: indexOfMatchLabelMapping;



include "params.mzn";



%=====================
% INTERNAL PARAMETERS
%=====================

% Number of indices in the match-to-destination label node mappings
% (a negative value indicates absence of a valid index)
int: numMatchLabelMappings = max([ i | i in indexOfMatchLabelMapping ]) + 1;

% Total number of registers values (additional values will be needed for
% representing certain settings, such as whether the value is an intermediate
% data value or an immediate).
int: numRegValues = numRegisters + 2;

% Total number of labels values (an additional value will be needed for
% representing the dummy label, to which non-selected matches will be
% allocated).
int: numLabelValues = numLabelsInFunction + 1;

% Reference to the dummy-label value
int: labelValueForDummy = numLabelValues - 1;

% Register values
% Reference to use when a data node represents an no-reuse value
int: regValueForNoReuse = numRegValues - 2;
% Reference to use when a data node represents an immediate value
int: regValueForImm = numRegValues - 1;

% Total number of match values (an additional value will be needed for
% indicating that the match is inactive).
int: numMatchValues = numMatches + 1;

% Reference to the inactive-match value
int: matchValueForInactive = numMatchValues - 1;

% Extends the external parameter 'execFrequencyOfLabelInFunction' with a value
% of 0 for the dummy label
array[allLabelsInFunctionPlusDummy] of int: execFrequencyOfLabelsPlusDummy =
  array1d( allLabelsInFunctionPlusDummy
         , execFrequencyOfLabelInFunction
           ++
           array1d(labelValueForDummy..labelValueForDummy, [0])
         );

% Sets to be used as array ranges
set of int: allOperationsInFunction = 0..numOpsInFunction-1;
set of int: allDataInFunction = 0..numDataInFunction-1;
set of int: allStatesInFunction = 0..numStatesInFunction-1;
set of int: allLabelsInFunction = 0..numLabelsInFunction-1;
set of int: allLabelsInFunctionPlusDummy = 0..numLabelValues-1;
set of int: allMatches = 0..numMatches-1;
set of int: allMatchesPlusInactive = 0..numMatchValues-1;
set of int: allRegValues = 0..numRegValues-1;
set of int: allIndicesInMatchLabelMappings = 0..numMatchLabelMappings-1;



%===========
% VARIABLES
%===========

% Operation node coverage
array[allOperationsInFunction] of var allMatchesPlusInactive: cov;

% Data node definitions, register allocation, and immediate values
array[allDataInFunction] of var allMatchesPlusInactive: ddefm;
array[allDataInFunction] of var set of allLabelsInFunctionPlusDummy: ddefbb;
array[allDataInFunction] of var allRegValues: reg;
array[allDataInFunction] of var int: cnst;

% State node definitions
array[allStatesInFunction] of var allMatchesPlusInactive: sdefm;
array[allStatesInFunction] of var set of allLabelsInFunctionPlusDummy: sdefbb;

% Match selection
array[allMatches] of var bool: sel;

% Basic block allocation of matches
array[allMatches] of var allLabelsInFunctionPlusDummy: bb;

% Basic block ordering. In the first variable (ord) each index represents a
% position (and the value the label put at that position), and the second
% variable (ord_inv) is the inverse of the first variable (that is, each
% index represents a label, and a value its position).
array[allLabelsInFunctionPlusDummy] of var allLabelsInFunctionPlusDummy: ord;
array[allLabelsInFunctionPlusDummy] of var allLabelsInFunctionPlusDummy:
  ord_inv;

% Branch-to-label distances
array[allIndicesInMatchLabelMappings] of var int: dist;

% Cost
var int: cost;



%====================
% Global constraints
%====================

include "alldifferent.mzn";
include "global_cardinality.mzn";
include "inverse.mzn";



%============
% CONSTRAINTS
%============

% Restrict the initial domains of 'cov', 'ddefm', and 'sdefm' such that matches
% are not allowed to cover or define nodes which are not part of the match
constraint
  forall (m in allMatches)
  (
    forall ( n in allOperationsInFunction
             where not (n in opsCoveredByMatch[m])
           )
    (
      cov[n] != m
    )
    /\
    forall (n in allDataInFunction where not (n in dataDefinedByMatch[m]))
    (
      ddefm[n] != m
    )
    /\
    forall (n in allStatesInFunction where not (n in statesDefinedByMatch[m]))
    (
      sdefm[n] != m
    )
  );

% Either all or none of the operations of match must be covered.
constraint
  let { array[1..numMatches] of var int: counts }
  in forall (m in allMatches)
     (
       counts[m+1] == 0 \/ counts[m+1] == card(opsCoveredByMatch[m])
     )
     /\
     global_cardinality(cov, [ m | m in allMatches ], counts);

% Essential operations must be covered by some match.
constraint
  forall (o in essentialOperationsInFunction)
  (
    cov[o] != matchValueForInactive
  );

% A match is selected if and only if all of its operation nodes are covered and
% its definable data and state nodes are defined by that pattern.
constraint
  forall (m in allMatches)
  (
    sel[m]
    <->
    (
      forall (n in opsCoveredByMatch[m])
      (
          cov[n] == m
      )
      /\
      forall (n in dataDefinedByMatch[m])
      (
          ddefm[n] == m
      )
      /\
      forall (n in statesDefinedByMatch[m])
      (
          sdefm[n] == m
      )
    )
  );

% Non-selected matches must be allocated to the dummy block.
constraint
  forall (m in allMatches)
  (
    not sel[m] <-> bb[m] == labelValueForDummy
  );

% Selected matches which have a root label must be allocated in the basic block
% of that label.
constraint
  forall (m in allMatches)
  (
    % If a match has no root label, then this set will be empty and hence there
    % will be no such constraint.
    forall (r in rootLabelOfMatch[m])
    (
      sel[m] -> bb[m] == r
    )
  );

% Entities defined by a selected match must be defined in the basic blocks
% consisting of the union between the basic block to which the match is
% allocated and the set of basic blocks which appears as label nodes in the
% pattern.
constraint
  forall (m in allMatches)
  (
    sel[m]
    ->
    ( forall (e in dataDefinedByMatch[m])
      (
        let { var set of allLabelsInFunction: bb_m_as_set
            , var set of allLabelsInFunction: labels_in_m
            }
        in bb[m] in bb_m_as_set /\ card(bb_m_as_set) == 1
           /\
           labels_in_m == nonRootLabelsInMatch[m]
           /\
           ddefbb[e] == (bb_m_as_set union labels_in_m)
      )
    )
  );

% An entity with a definition placement edge to a label must be defined within
% the basic block of that label.
constraint
  forall (e in allDataInFunction)
  (
    forall (l in labelsWhereDataInFunctionMustBeDefined[e])
    (
      l in ddefbb[e]
    )
  )
  /\
  forall (e in allStatesInFunction)
  (
    forall (l in labelsWhereStatesInFunctionMustBeDefined[e])
    (
      l in sdefbb[e]
    )
  );

% Inactive entities must not be defined by any match nor be defined in any basic
% block.
constraint
  forall (e in allDataInFunction)
  (
    ddefm[e] == matchValueForInactive
    <->
    ddefbb[e] == {labelValueForDummy}
  )
  /\
  forall (e in allStatesInFunction)
  (
    sdefm[e] == matchValueForInactive
    <->
    sdefbb[e] == {labelValueForDummy}
  );

% Enforce that all selected matches must be allocated to a basic block such that
% every used data node is dominated by its data definition. Likewise for the
% state nodes. These constraints should not be applied to the generic phi
% patterns.
constraint
  forall (m in allMatches where applyDefDomUseConstraintForMatch[m]) (
    sel[m]
    ->
    (
      forall (e in dataUsedInMatch[m])
      (
        exists (l in allLabelsInFunctionPlusDummy)
        (
          l in ddefbb[e]
          /\
          l in domsetOfLabelInFunction[bb[m]]
        )
      )
      /\
      forall (e in statesUsedInMatch[m]) (
        exists (l in allLabelsInFunctionPlusDummy)
        (
          l in ddefbb[e]
          /\
          l in domsetOfLabelInFunction[bb[m]]
        )
      )
    )
  );

% The basic blocks must be serialized in some order.
constraint
  alldifferent(ord);

% Apply the channel constraint between 'ord' and 'ord_inv'.
constraint
  inverse(ord, ord_inv);

% The root label (i.e. function entry point) must be placed as the first basic
% block, and the dummy label must be placed as the last basic block.
constraint
  ord[rootLabelOfFunction] == 0;
constraint
  ord[labelValueForDummy] == numLabelValues - 1;

% Calculate distances between matches (where appropriate) and labels.
constraint
  forall ( m in allMatches
         , l in allLabelsInFunction
           where indexOfMatchLabelMapping[m, l] >= 0
         )
  (
    (
      sel[m]
      ->
      dist[indexOfMatchLabelMapping[m, l]] =
        % When the label appears after the branch
        sum (i in allLabelsInFunction)
        (
          % Assuming the branch always appears as the last instruction within
          % the block, which means that we only need to consider the size of
          % the blocks between the source and the target label.
          sum (mm in allMatches)
          (
            codeSizeOfMatch[mm] * bool2int (bb[mm] == i)
          )
          *
          bool2int
            (  ord_inv[bb[m]] < ord_inv[i]
            /\ ord_inv[i] < ord_inv[l]
            )
        )
        -
        % When the label appears before the branch
        sum (i in allLabelsInFunction)
        (
          % Assuming the branch always appears as the last instruction within
          % the block, which means that we consider all instructions in the
          % block wherein the branch appears as well as the size of the blocks
          % between the source and the target label.
          sum (mm in allMatches)
          (
            codeSizeOfMatch[mm] * bool2int (bb[mm] == i)
          )
          *
          bool2int
            (  ord_inv[l] <= ord_inv[i]
            /\ ord_inv[i] <= ord_inv[bb[m]]
            )
        )
    )
    /\
    (
      not sel[m]
      ->
      dist[indexOfMatchLabelMapping[m, l]] = 0
    )
  );

% Constraint the immediate value for all data nodes which do not represent
% immediate values.
constraint
  forall (n in allDataInFunction)
  (
    reg[n] != regValueForImm -> cnst[n] == 0
  );

% Accumulate cost of selected patterns.
% TODO: make this part generic
constraint
  cost = sum (m in allMatches)
         ( bool2int(sel[m])
         * latencyOfMatch[m]
         * execFrequencyOfLabelsPlusDummy[bb[m]]
         );



%==================
% SOLVE AND OUTPUT
%==================

solve ::
seq_search
  ( % Improves solving time
    [ int_search (cov, first_fail, indomain_min, complete) ]
  )
minimize cost;

output [ "cov=", show(cov), "\n"
       , "ddefm=", show(ddefm), "\n"
       , "ddefbb=", show(ddefbb), "\n"
       , "reg=", show(reg), "\n"
       , "cnst=", show(cnst), "\n"
       , "sdefm=", show(sdefm), "\n"
       , "sdefbb=", show(sdefbb), "\n"
       , "sel=", show(sel), "\n"
       , "bb=", show(bb), "\n"
       , "ord=", show(ord), "\n"
       , "dist=", show(dist), "\n"
       , "label_value_for_dummy=", show(labelValueForDummy), "\n"
       , "reg_value_for_no_reuse=", show(regValueForNoReuse), "\n"
       , "reg_value_for_imm=", show(regValueForImm), "\n"
       , "cost=", show(cost), "\n"
       ];
