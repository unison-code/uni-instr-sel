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
int: numBlocksInFunction;
int: entryBlockOfFunction;
array[allBlocksInFunction] of set of int: domSetOfBlockInFunction;
array[allBlocksInFunction] of set of int: defEdgesForBlockInFunction;
array[int] of allEntitiesInFunction: stateEntitiesInFunction;
array[allBlocksInFunction] of int: execFrequencyOfBlockInFunction;

% Target machine data.
int: numLocations;

% Match data.
int: numMatches;
array[allMatches] of set of int: operationsCoveredByMatch;
array[allMatches] of set of int: entitiesDefinedByMatch;
array[allMatches] of set of int: entitiesUsedByMatch;
array[allMatches] of set of int: entryBlockOfMatch;
array[allMatches] of set of int: nonEntryBlocksInMatch;
array[allMatches] of int: codeSizeOfMatch;
array[allMatches] of int: latencyOfMatch;
array[allMatches] of bool: applyDefDomUseConstraintForMatch;
set of allMatches: nonCopyMatches;

% These variables will be set by concatenating the definitions to the end of
% this file.



%=====================
% INTERNAL PARAMETERS
%=====================

% Total number of location values (an additional value will be needed for
% representing the null location, for when the entity cannot be reused by other
% matches).
int: numLocValues = numLocations + 1;

% Total number of blocks values (an additional value will be needed for
% representing the null block, to which non-selected matches will be moved).
int: numBlockValues = numBlocksInFunction + 1;

% Reference to the null-block value.
int: blockValueForNull = numBlockValues - 1;

% Reference to to the null-location value.
int: locValueForNull = numLocValues - 1;

% Extends the external parameter 'domSetOfBlockInFunction' with a value
% for the null block (which is assumed to be dominated by all blocks).
array[allBlocksInFunctionPlusNull] of set of int:
  domSetOfBlockInFunctionPlusNull =
    array1d( allBlocksInFunctionPlusNull
           , domSetOfBlockInFunction
             ++
             array1d(blockValueForNull..blockValueForNull, [allBlocksInFunction])
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
set of int: allEntitiesInFunction = 0..numEntitiesInFunction-1;
set of int: allBlocksInFunction = 0..numBlocksInFunction-1;
set of int: allBlocksInFunctionPlusNull = 0..numBlockValues-1;
set of int: allMatches = 0..numMatches-1;
set of int: allLocValues = 0..numLocValues-1;



%===========
% VARIABLES
%===========

% Entity node definitions and locations.
array[allEntitiesInFunction] of var allBlocksInFunction: def;
array[allEntitiesInFunction] of var allLocValues: loc;

% Match selection.
array[allMatches] of var bool: sel;

% Basic block to which the matches are moved.
array[allMatches] of var allBlocksInFunctionPlusNull: mov;

% Block ordering (succ[b] is the block appearing immeditely after block b in the
% generated code).
array[allBlocksInFunctionPlusNull] of var allBlocksInFunctionPlusNull: succ;

% Cost.
var int: cost;



%====================
% Global constraints
%====================

include "circuit.mzn";
include "table.mzn";



%===========
% PREDICATES
%===========

% Fall-through constraint: the block to which match m is moved must be
% immediately followed by block b.
predicate fall_through(int: m, int: b) =
(
  succ[mov[m]] = b
);



%============
% CONSTRAINTS
%============

% Enforce that, for each operation, exactly one match must be selected such that
% the operation is covered.
constraint
  forall (o in allOperationsInFunction)
  (
    let {
      set of int: mset = { m | m in allMatches
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

% Enforce that, for each entity, exactly one match must be selected such that
% the entity is defined.
%
% This is an implied constraint, but it also enforces that the patterns for
% defining the function input and constants are selected. Such patterns do not
% cover any operations, they are not entailed in the above constraint for
% exactly covering each operation.
constraint
  forall (e in allEntitiesInFunction)
  (
    let {
      set of int: mset = { m | m in allMatches
                                    where e in entitiesDefinedByMatch[m]
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

% Implied: The total number of entities defined by the selected matches must be
% equal to the number of entities in the function graph (Mats calls this
% "minimal effect").
constraint
  sum (m in allMatches)
  (
    card(entitiesDefinedByMatch[m]) * bool2int(sel[m])
  ) = numEntitiesInFunction;

% Selected matches must not be moved to the null block.
constraint
  forall (m in allMatches)
  (
    sel[m] <-> mov[m] != blockValueForNull
  );

% Selected matches that have an entry block must be moved in the basic block of
% that block.
constraint
  forall (m in allMatches)
  (
    % If a match has no entry block, then this set will be empty and hence there
    % will be no such constraint. It is assumed that there will be at most one
    % entry.
    forall (r in entryBlockOfMatch[m])
    (
      sel[m] -> mov[m] = r
    )
  );

% Entities defined by a selected match must be defined in one of the basic
% blocks that the selected match spans over.
constraint
  forall (m in allMatches)
  (
    forall (e in entitiesDefinedByMatch[m])
    (
      if (card(entryBlockOfMatch[m]) > 0)
      then
        let { set of allBlocksInFunction: blocks_in_m =
                entryBlockOfMatch[m] union nonEntryBlocksInMatch[m]
            }
        in sel[m] -> def[e] in blocks_in_m
      else
        sel[m] -> def[e] = mov[m]
      endif
    )
  );

% For every block wherein an entity is defined, there must exist some selected
% match such that it is either placed in that block or that block is part of
% one of the blocks that appear in the selected match.
%
% I am not certain whether this is a strictly required or just an implied
% constraint...
constraint
  forall (e in allEntitiesInFunction, l in allBlocksInFunction)
  (
    l = def[e]
    ->
    exists (m in allMatches)
    (
      l = mov[m] \/ (sel[m] /\ l in nonEntryBlocksInMatch[m])
    )
  );

% An entity with a definition edge with a block must be defined in the basic
% block of that block.
constraint
  forall (l in allBlocksInFunction)
  (
    forall (e in defEdgesForBlockInFunction[l])
    (
      def[e] = l
    )
  );

% Enforce that every entity is defined in a block such that the block dominates
% all blocks wherein the entity is used. This constraint shall not be applied to
% the generic phi patterns.
%
% The code below is essentially a more efficient implementation of:
%     constraint
%       forall ( m in allMatches, e in entitiesUsedByMatch[m]
%                     where applyDefDomUseConstraintForMatch[m]
%              )
%       (
%         def[e] in domSetOfBlockInFunctionPlusNull[mov[m]]
%       );
int: DomRelSize =
  sum (l in allBlocksInFunction)
  (
    card(domSetOfBlockInFunction[l])
  ) + numBlocksInFunction;
array[1..DomRelSize, 1..2] of allBlocksInFunctionPlusNull: DomRel =
  array2d(1..DomRelSize, 1..2,
    [ if k=1 then i else j endif | i in allBlocksInFunctionPlusNull,
                                   j in domSetOfBlockInFunctionPlusNull[i],
                                   k in 1..2
    ]);
constraint
  forall ( m in allMatches, e in entitiesUsedByMatch[m]
                where applyDefDomUseConstraintForMatch[m]
         )
  (
    table([mov[m], def[e]], DomRel)
  );

% Ensure that succ forms a circuit (thus resulting in an ordering of blocks).
constraint
  circuit(succ) :: domain;

% The block of the entry block (i.e. function entry point) must be placed as the
% first block, and the block of the null block must be placed as the last block.
constraint
  succ[blockValueForNull] = entryBlockOfFunction;

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
         ( latencyOfMatch[m]
         * execFrequencyOfBlocksPlusNull[mov[m]]
         );



%==================
% SOLVE AND OUTPUT
%==================

solve ::
  seq_search(
    [ bool_search([ sel[m] | m in nonCopyMatches
                                  where card(operationsCoveredByMatch[m])
                                        +
                                        card(entitiesDefinedByMatch[m])
                                        > 2
                  ], input_order, indomain_max, complete)
    , bool_search([ sel[m] | m in nonCopyMatches
                                  where card(operationsCoveredByMatch[m])
                                        +
                                        card(entitiesDefinedByMatch[m])
                                        = 2
				  ], input_order, indomain_max, complete)
    , bool_search([ sel[m] | m in nonCopyMatches
                                  where card(operationsCoveredByMatch[m])
                                        +
                                        card(entitiesDefinedByMatch[m])
                                        = 1
				  ], input_order, indomain_max, complete)
	]
  )
minimize cost;

output [ "sel=", show(sel), "\n"
       , "def=", show(def), "\n"
       , "loc=", show(loc), "\n"
       , "mov=", show(mov), "\n"
       , "succ=", show(succ), "\n"
       , "block_value_for_null=", show(blockValueForNull), "\n"
       , "loc_value_for_null=", show(locValueForNull), "\n"
       , "cost=", show(cost), "\n"
       ];
