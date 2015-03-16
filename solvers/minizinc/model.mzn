% Copyright (c) 2013-2015, Gabriel Hjort Blindell <ghb@kth.se>
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
int: numEntitiesInFunction;
int: numLabelsInFunction;
int: entryLabelOfFunction;
array[allLabelsInFunction] of set of int: domSetOfLabelInFunction;
array[allLabelsInFunction] of set of int: invDomSetOfLabelInFunction;
array[allLabelsInFunction] of set of int: domEdgesFromLabelInFunction;
array[allLabelsInFunction] of set of int: domEdgesToLabelInFunction;
array[int] of allEntitiesInFunction: stateEntitiesInFunction;
array[int] of allOperationsInFunction: essentialOperationsInFunction;
array[allLabelsInFunction] of int: execFrequencyOfLabelInFunction;

% Target machine data.
int: numRegisters;

% Match data.
int: numMatches;
array[allMatches] of set of int: operationsCoveredByMatch;
array[allMatches] of set of int: entitiesDefinedByMatch;
array[allMatches] of set of int: entitiesUsedByMatch;
array[allMatches] of set of int: entryLabelOfMatch;
array[allMatches] of set of int: nonEntryLabelsInMatch;
array[allMatches] of int: codeSizeOfMatch;
array[allMatches] of int: latencyOfMatch;
array[allMatches] of bool: applyDefDomUseConstraintForMatch;
array[allMatches, allLabelsInFunction] of int: indexOfMatchLabelMapping;



include "params.mzn";



%=====================
% INTERNAL PARAMETERS
%=====================

% Number of indices in the match-to-destination label node mappings
% (a negative value indicates absence of a valid index).
int: numMatchLabelMappings = max( [ indexOfMatchLabelMapping[i,j]
                                  | i in allMatches
                                  , j in allLabelsInFunction
                                  ]
                                ) + 1;

% Total number of registers values (additional values will be needed for
% representing certain settings, such as whether the value is an intermediate
% data value or an immediate).
int: numRegValues = numRegisters + 2;

% Total number of labels values (an additional value will be needed for
% representing the dummy label, to which non-selected matches will be
% allocated).
int: numLabelValues = numLabelsInFunction + 1;

% Reference to the dummy-label value.
int: labelValueForDummy = numLabelValues - 1;

% Register values.

% Reference to use when a data node represents an no-reuse value.
int: regValueForNoReuse = numRegValues - 2;
% Reference to use when a data node represents an immediate value.
int: regValueForImm = numRegValues - 1;

% Total number of match values (an additional value will be needed for
% indicating that the match is inactive).
int: numMatchValues = numMatches + 1;

% Reference to the inactive-match value.
int: matchValueForInactive = numMatchValues - 1;

% Extends the external parameter 'execFrequencyOfLabelInFunction' with a value
% of 0 for the dummy label.
array[allLabelsInFunctionPlusDummy] of int: execFrequencyOfLabelsPlusDummy =
  array1d( allLabelsInFunctionPlusDummy
         , execFrequencyOfLabelInFunction
           ++
           array1d(labelValueForDummy..labelValueForDummy, [0])
         );

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 0..numOperationsInFunction-1;
set of int: allEntitiesInFunction = 0..numEntitiesInFunction-1;
set of int: allLabelsInFunction = 0..numLabelsInFunction-1;
set of int: allLabelsInFunctionPlusDummy = 0..numLabelValues-1;
set of int: allMatches = 0..numMatches-1;
set of int: allMatchesPlusInactive = 0..numMatchValues-1;
set of int: allRegValues = 0..numRegValues-1;
set of int: allIndicesInMatchLabelMappings = 0..numMatchLabelMappings-1;



%===========
% VARIABLES
%===========

% Operation node coverage.
array[allOperationsInFunction] of var allMatchesPlusInactive: cov;

% Entity node definitions, register allocation, and immediate values.
array[allEntitiesInFunction] of var allMatchesPlusInactive: defm;
array[allEntitiesInFunction] of var set of allLabelsInFunctionPlusDummy: defbb;
array[allEntitiesInFunction] of var allRegValues: reg;
array[allEntitiesInFunction] of var int: cnst;

% Match selection.
array[allMatches] of var bool: sel;

% Basic block allocation of matches.
array[allMatches] of var allLabelsInFunctionPlusDummy: bb;

% Basic block ordering. In the first variable (ord) each index represents a
% position (and the value the label put at that position), and the second
% variable (ord_inv) is the inverse of the first variable (that is, each
% index represents a label, and a value its position).
array[allLabelsInFunctionPlusDummy] of var allLabelsInFunctionPlusDummy: ord;
array[allLabelsInFunctionPlusDummy] of var allLabelsInFunctionPlusDummy:
  ord_inv;

% Branch-to-label distances.
array[allIndicesInMatchLabelMappings] of var int: dist;

% Cost.
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

% Restrict the initial domains of 'cov' and 'defm' such that matches are not
% allowed to cover or define nodes which are not part of the match.
constraint
  forall (m in allMatches)
  (
    forall ( o in allOperationsInFunction
             where not (o in operationsCoveredByMatch[m])
           )
    (
      cov[o] != m
    )
    /\
    forall ( e in allEntitiesInFunction
             where not (e in entitiesDefinedByMatch[m])
           )
    (
      defm[e] != m
    )
  );

% Either all or none of the operations of match must be covered.
constraint
  let { array[1..numMatches] of var int: counts }
  in forall (m in allMatches)
     (
       let { int: i = m + 1 }
       in counts[i] == 0 \/ counts[i] == card(operationsCoveredByMatch[m])
     )
     /\
     global_cardinality(cov, [ m | m in allMatches ], counts);

% A match is selected if and only if some operation is covered by that match, or
% if some entity is defined by that match.
constraint
  forall (m in allMatches)
  (
    sel[m]
    <->
    (
      exists (o in operationsCoveredByMatch[m])
      (
          cov[o] == m
      )
      \/
      exists (e in entitiesDefinedByMatch[m])
      (
          defm[e] == m
      )
    )
  );

% Selected matches must define all of its entities.
constraint
  forall (m in allMatches)
  (
    sel[m]
    ->
    (
      forall (e in entitiesDefinedByMatch[m])
      (
          defm[e] == m
      )
    )
  );

% Essential operations must be covered by some match.
constraint
  forall (o in essentialOperationsInFunction)
  (
    cov[o] != matchValueForInactive
  );

% Non-selected matches must be allocated to the dummy block.
constraint
  forall (m in allMatches)
  (
    not sel[m] <-> bb[m] == labelValueForDummy
  );

% Selected matches that have an entry label must be allocated in the basic block
% of that label.
constraint
  forall (m in allMatches)
  (
    % If a match has no entry label, then this set will be empty and hence there
    % will be no such constraint. It is assumed that there will be at most one
    % entry.
    forall (r in entryLabelOfMatch[m])
    (
      sel[m] -> bb[m] == r
    )
  );

% Entities defined by a selected match must be defined in the basic blocks
% consisting of the union between the basic block to which the match is
% allocated and the set of basic blocks that appear as label nodes in the
% pattern. Due to the restrctions of Minizinc, however, we need to reformulate
% this constraint a bit in order to achieve this effect.
constraint
  forall (m in allMatches)
  (
    sel[m]
    ->
    forall (e in entitiesDefinedByMatch[m])
    (
      if (card(entryLabelOfMatch[m]) > 0)
      then
        let { set of allLabelsInFunction: labels_in_m =
                entryLabelOfMatch[m] union nonEntryLabelsInMatch[m]
            }
        in defbb[e] == labels_in_m
      else
        bb[m] in defbb[e] /\ card(defbb[e]) == 1
      endif
    )
  );

% For every label wherein an entity is defined, there must exist some selected
% match such that it is either placed in that label or that label is part of
% one of the labels that appear in the selected match.
constraint
  forall (e in allEntitiesInFunction)
  (
    forall (l in allLabelsInFunction)
    (
      l in defbb[e]
      ->
      exists (m in allMatches)
      (
        sel[m]
        /\
        (l == bb[m] \/ l in nonEntryLabelsInMatch[m])
      )
    )
  );

% An entity with a dominance edge to a label must be defined within some basic
% block that dominates the basic block of the label that participates in the
% edge.
constraint
  forall (l in allLabelsInFunction)
  (
    forall (e in domEdgesToLabelInFunction[l])
    (
      exists (k in allLabelsInFunction)
      (
        k in defbb[e]
        /\
        k in domSetOfLabelInFunction[l]
      )
    )
  );

% An entity with a dominance edge from a label must be defined within some basic
% block that is dominated by the basic block of the label that participates in
% the edge.
constraint
  forall (l in allLabelsInFunction)
  (
    forall (e in domEdgesFromLabelInFunction[l])
    (
      exists (k in allLabelsInFunction)
      (
        k in defbb[e]
        /\
        k in invDomSetOfLabelInFunction[l]
      )
    )
  );

% Inactive entities must not be defined by any match nor be defined in any
% (real) basic block.
constraint
  forall (e in allEntitiesInFunction)
  (
    defm[e] == matchValueForInactive
    <->
    defbb[e] == {labelValueForDummy}
  );

% Enforce that, for every selected match, every use of an entity has a
% definition such that the defintion dominates the use. These constraints shall
% not be applied to the generic phi patterns.
constraint
  forall (m in allMatches where applyDefDomUseConstraintForMatch[m])
  (
    sel[m]
    ->
    forall (e in entitiesUsedByMatch[m])
    (
      exists (l in allLabelsInFunctionPlusDummy)
      (
        l in defbb[e]
        /\
        l in domSetOfLabelInFunction[bb[m]]
      )
    )
  );

% The basic blocks must be serialized in some order.
constraint
  alldifferent(ord);

% Apply the channel constraint between 'ord' and 'ord_inv'.
constraint
  inverse(ord, ord_inv);

% The entry label (i.e. function entry point) must be placed as the first basic
% block, and the dummy label must be placed as the last basic block.
constraint
  ord[entryLabelOfFunction] == 0;
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
        % When the label appears after the branch.
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
        % When the label appears before the branch.
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

% Constrain the reg value for all entities that are state nodes.
constraint
  forall (e in stateEntitiesInFunction)
  (
    reg[e] = regValueForImm
  );

% Constrain the immediate value for all entities which do not represent
% immediate values.
constraint
  forall (e in allEntitiesInFunction)
  (
    reg[e] != regValueForImm -> cnst[e] == 0
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
  ( % Improves solving time.
    [ bool_search (sel, first_fail, indomain_min, complete)
    , int_search (bb, first_fail, indomain_min, complete)
    , int_search (ord, first_fail, indomain_min, complete)
    ]
  )
minimize cost;

output [ "cov=", show(cov), "\n"
       , "defm=", show(defm), "\n"
       , "defbb=", show(defbb), "\n"
       , "reg=", show(reg), "\n"
       , "cnst=", show(cnst), "\n"
       , "sel=", show(sel), "\n"
       , "bb=", show(bb), "\n"
       , "ord=", show(ord), "\n"
       , "dist=", show(dist), "\n"
       , "label_value_for_dummy=", show(labelValueForDummy), "\n"
       , "reg_value_for_no_reuse=", show(regValueForNoReuse), "\n"
       , "reg_value_for_imm=", show(regValueForImm), "\n"
       , "cost=", show(cost), "\n"
       ];
