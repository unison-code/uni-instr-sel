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
array[allLabelsInFunction] of int: execFrequencyOfLabelInFunction;

% Target machine data.
int: numLocations;

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

% Total number of location values (an additional value will be needed for
% representing the null location, for when the entity cannot be reused by other
% matches).
int: numLocValues = numLocations + 1;

% Total number of labels values (an additional value will be needed for
% representing the null label, to which non-selected matches will be moved).
int: numLabelValues = numLabelsInFunction + 1;

% Reference to the null-label value.
int: labelValueForNull = numLabelValues - 1;

% Reference to to the null-location value.
int: locValueForNull = numLocValues - 1;

% Extends the external parameter 'execFrequencyOfLabelInFunction' with a value
% of 0 for the null label.
array[allLabelsInFunctionPlusNull] of int: execFrequencyOfLabelsPlusNull =
  array1d( allLabelsInFunctionPlusNull
         , execFrequencyOfLabelInFunction
           ++
           array1d(labelValueForNull..labelValueForNull, [0])
         );

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 0..numOperationsInFunction-1;
set of int: allEntitiesInFunction = 0..numEntitiesInFunction-1;
set of int: allLabelsInFunction = 0..numLabelsInFunction-1;
set of int: allLabelsInFunctionPlusNull = 0..numLabelValues-1;
set of int: allMatches = 0..numMatches-1;
set of int: allLocValues = 0..numLocValues-1;
set of int: allIndicesInMatchLabelMappings = 0..numMatchLabelMappings-1;



%===========
% VARIABLES
%===========

% Entity node definitions and locations.
array[allEntitiesInFunction] of var allLabelsInFunction: def;
array[allEntitiesInFunction] of var allLocValues: loc;

% Match selection.
array[allMatches] of var bool: sel;

% Basic block to which the matches are moved.
array[allMatches] of var allLabelsInFunctionPlusNull: mov;

% Basic block ordering. In the first variable (ord) each index represents a
% position (and the value the label put at that position), and the second
% variable (ord_inv) is the inverse of the first variable (that is, each
% index represents a label, and a value its position).
array[allLabelsInFunctionPlusNull] of var allLabelsInFunctionPlusNull: ord;
array[allLabelsInFunctionPlusNull] of var allLabelsInFunctionPlusNull:
  ord_inv;

% Branch-to-label distances.
array[allIndicesInMatchLabelMappings] of var int: dist;

% Cost.
var int: cost;



%====================
% Global constraints
%====================

include "alldifferent.mzn";
include "exactly.mzn";
include "inverse.mzn";



%============
% CONSTRAINTS
%============

% Enforce that, for each operation, exactly one match must be selected such that
% the operation is covered.
constraint
  forall (o in allOperationsInFunction)
  (
    let {
      array[int] of var bool:
      matches = [ sel[m] | m in allMatches
                           where o in operationsCoveredByMatch[m]
                ]
    }
    in exactly(1, matches, 1)
  );

% Non-selected matches must be moved to the null block.
constraint
  forall (m in allMatches)
  (
    not sel[m] <-> mov[m] == labelValueForNull
  );

% Selected matches that have an entry label must be moved in the basic block of
% that label.
constraint
  forall (m in allMatches)
  (
    % If a match has no entry label, then this set will be empty and hence there
    % will be no such constraint. It is assumed that there will be at most one
    % entry.
    forall (r in entryLabelOfMatch[m])
    (
      sel[m] -> mov[m] == r
    )
  );

% Entities defined by a selected match must be defined in one of the basic
% blocks that the selected match spans over.
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
        in def[e] in labels_in_m
      else
        def[e] == mov[m]
      endif
    )
  );

% For every label wherein an entity is defined, there must exist some selected
% match such that it is either placed in that label or that label is part of
% one of the labels that appear in the selected match.
%
% I am not certain whether this is a strictly required or just an implied
% constraint...
constraint
  forall (e in allEntitiesInFunction)
  (
    forall (l in allLabelsInFunction)
    (
      l == def[e]
      ->
      exists (m in allMatches)
      (
        sel[m]
        /\
        (l == mov[m] \/ l in nonEntryLabelsInMatch[m])
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
      def[e] in domSetOfLabelInFunction[l]
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
      def[e]in invDomSetOfLabelInFunction[l]
    )
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
      exists (l in allLabelsInFunctionPlusNull)
      (
        l == def[e]
        /\
        l in domSetOfLabelInFunction[mov[m]]
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
% block, and the null label must be placed as the last basic block.
constraint
  ord[entryLabelOfFunction] == 0;
constraint
  ord[labelValueForNull] == numLabelValues - 1;

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
            codeSizeOfMatch[mm] * bool2int (mov[mm] == i)
          )
          *
          bool2int
            (  ord_inv[mov[m]] < ord_inv[i]
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
            codeSizeOfMatch[mm] * bool2int (mov[mm] == i)
          )
          *
          bool2int
            (  ord_inv[l] <= ord_inv[i]
            /\ ord_inv[i] <= ord_inv[mov[m]]
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

% Constrain the loc value for all entities that are state nodes.
constraint
  forall (e in stateEntitiesInFunction)
  (
    loc[e] = locValueForNull
  );

% Accumulate cost of selected patterns.
% TODO: make this part generic
constraint
  cost = sum (m in allMatches)
         ( bool2int(sel[m])
         * latencyOfMatch[m]
         * execFrequencyOfLabelsPlusNull[mov[m]]
         );



%==================
% SOLVE AND OUTPUT
%==================

solve ::
seq_search
  ( % Improves solving time.
    [ bool_search (sel, first_fail, indomain_min, complete)
    , int_search (mov, first_fail, indomain_min, complete)
    , int_search (ord, first_fail, indomain_min, complete)
    ]
  )
minimize cost;

output [ "sel=", show(sel), "\n"
       , "def=", show(def), "\n"
       , "loc=", show(loc), "\n"
       , "mov=", show(mov), "\n"
       , "ord=", show(ord), "\n"
       , "dist=", show(dist), "\n"
       , "label_value_for_null=", show(labelValueForNull), "\n"
       , "loc_value_for_null=", show(locValueForNull), "\n"
       , "cost=", show(cost), "\n"
       ];
