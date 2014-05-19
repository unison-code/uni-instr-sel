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
int: numFuncActionNodes;
int: numFuncDataNodes;
int: numFuncStateNodes;
int: numFuncLabelNodes;
int: rootLabel;
array[allFuncLabelNodes] of set of int: funcLabelDomsets;

% Target machine data
int: numRegisters;

% Pattern instance data
int: numPatternInstances;
array[allPatternInstances] of set of int: patInstActionsCovered;
array[allPatternInstances] of set of int: patInstDataDefined;
array[allPatternInstances] of set of int: patInstDataUsed;
array[allPatternInstances] of set of int: patInstStateDefined;
array[allPatternInstances] of set of int: patInstStateUsed;
array[allPatternInstances, allFuncLabelNodes] of int: patInstAndLabelMappings;
array[allPatternInstances] of int: patInstCodeSizes;
array[allPatternInstances] of int: patInstLatencies;
array[allPatternInstances] of bool: patInstNoUseDefDomConstraints;



include "input.mzn";



%=====================
% INTERNAL PARAMETERS
%=====================

% Number of indices in the pattern instance-to-destination label node mappings
% (a negative value indicates absence of a valid index)
int: numIndicesInPatInstLabelMappings =
     max([ i | i in patInstAndLabelMappings ]) + 1;

% Reference to the null label
int: nullLabel = numFuncLabelNodes;

% Register reference to use when a data node represents an immediate value
int: regForImmValues = numRegisters;

% Sets to be used as array ranges
set of int: allFuncActionNodes = 0..numFuncActionNodes-1;
set of int: allFuncDataNodes = 0..numFuncDataNodes-1;
set of int: allFuncStateNodes = 0..numFuncStateNodes-1;
set of int: allFuncLabelNodes = 0..numFuncLabelNodes-1;
set of int: allFuncLabelNodesPlusNull = 0..numFuncLabelNodes;
set of int: allPatternInstances = 0..numPatternInstances-1;
set of int: allRegisters = 0..numRegisters-1;
set of int: allIndicesInPatInstLabelMappings =
    0..numIndicesInPatInstLabelMappings-1;



%===========
% VARIABLES
%===========

% Action node coverage
array[allFuncActionNodes] of var allPatternInstances: an_cov;

% Data node definitions, register allocation, and immediate values
array[allFuncDataNodes] of var allPatternInstances: dn_def;
array[allFuncDataNodes] of var allRegisters: dn_reg;
array[allFuncDataNodes] of var int: dn_imm;

% State node definitions
array[allFuncStateNodes] of var allPatternInstances: sn_def;

% Pattern instance selection
array[allPatternInstances] of var bool: pi_sel;

% Basic block allocation of pattern instances
array[allPatternInstances] of var allFuncLabelNodesPlusNull: pi_bb;

% Basic block ordering. In the first variable (bb_order) each index represents a
% position (and the value the label put at that position), and the second
% variable (bb_order_inv) is the inverse of the first variable (that is, each
% index represents a label, and a value its position).
array[allFuncLabelNodesPlusNull] of var allFuncLabelNodesPlusNull: bb_order;
array[allFuncLabelNodesPlusNull] of var allFuncLabelNodesPlusNull: bb_order_inv;

% Branch-to-label distances
array[allIndicesInPatInstLabelMappings] of var int: br_bb_dists;

% Cost
var int: cost;



%====================
% Global constraints
%====================

include "alldifferent.mzn";
include "inverse.mzn";



%============
% CONSTRAINTS
%============

% Prevent a pattern from covering action nodes and defining data or state nodes
% outside its range.
constraint
forall (p in allPatternInstances)
(
    forall (n in allFuncActionNodes where not (n in patInstActionsCovered[p]))
    (
        an_cov[n] != p
    )
    /\
    forall (n in allFuncDataNodes where not (n in patInstDataDefined[p]))
    (
        dn_def[n] != p
    )
    /\
    forall (n in allFuncStateNodes where not (n in patInstStateDefined[p]))
    (
        sn_def[n] != p
    )
);

% Enforce selection of pattern if and only if an action node is covered or a
% data or state node is defined by that pattern.
constraint
forall (p in allPatternInstances)
(
    pi_sel[p]
    <->
    (
        forall (n in patInstActionsCovered[p])
        (
            an_cov[n] == p
        )
        /\
        forall (n in patInstDataDefined[p])
        (
            dn_def[n] == p
        )
        /\
        forall (n in patInstStateDefined[p])
        (
            sn_def[n] == p
        )
    )
);

% Allocate all non-selected pattern instances to a null label.
constraint
forall (p in allPatternInstances)
(
    not pi_sel[p] -> pi_bb[p] = nullLabel
);

% Enforce that all selected pattern instances must be allocated to a basic block
% such that every used data node is dominated by its data definition. Likewise
% for the state nodes. These constraints should not be applied to the generic
% phi patterns.
constraint
forall (p in allPatternInstances where not patInstNoUseDefDomConstraints[p])
(
    pi_sel[p]
    ->
    (
        forall (n in patInstDataUsed[p])
        (
            pi_bb[dn_def[n]] in funcLabelDomsets[pi_bb[p]]
        )
        /\
        forall (n in patInstStateUsed[p])
        (
            pi_bb[sn_def[n]] in funcLabelDomsets[pi_bb[p]]
        )
    )
);

% Enforce that all basic blocks are serialized in some order.
constraint
alldifferent(bb_order);

% Enforce the channel constraint between bb_order and bb_order_inv
constraint
inverse(bb_order, bb_order_inv);

% Enforce that the root label (i.e. function entry point) is set as the first
% block, and that the null label is set as the last block.
constraint
bb_order[rootLabel] == 0;
constraint
bb_order[nullLabel] == nullLabel;

% Calculate distances between pattern instances (where appropriate) and labels
constraint
forall (p in allPatternInstances, l in allFuncLabelNodes
        where patInstAndLabelMappings[p, l] >= 0)
(
    br_bb_dists[patInstAndLabelMappings[p, l]] =
        % When the label appears after the branch
        sum (i in allFuncLabelNodes)
        (
            % Assuming the branch always appears as the last instruction within
            % the block, which means that we only need to consider the size of
            % the blocks between the source and the target label.
            sum (pp in allPatternInstances)
            (
                patInstCodeSizes[pp] * bool2int(pi_bb[pp] == i)
            ) * bool2int(bb_order_inv[pi_bb[p]] + 1 <= bb_order_inv[i]
                         /\
                         bb_order_inv[i] < bb_order_inv[l])
        )
        -
        % When the label appears before the branch
        sum (i in allFuncLabelNodes)
        (
            % Assuming the branch always appears as the last instruction within
            % the block, which means that we consider all instructions in the
            % block wherein the branch appears as well as the size of the blocks
            % between the source and the target label.
            sum (pp in allPatternInstances)
            (
                patInstCodeSizes[pp] * bool2int(pi_bb[pp] == i)
            ) * bool2int(bb_order_inv[l] <= bb_order_inv[i]
                         /\
                         bb_order_inv[i] <= bb_order_inv[pi_bb[p]])
        )
);

% Force setting of the immediate value for all data nodes which do not represent
% immediate values
constraint
forall (n in allFuncDataNodes)
(
  dn_reg[n] == regForImmValues -> dn_imm[n] == 0
);

% Accumulate cost of selected patterns.
% TODO: make this part generic
constraint
cost = sum (p in allPatternInstances)
       (
           patInstLatencies[p] * bool2int(pi_sel[p])
       );



%==================
% SOLVE AND OUTPUT
%==================

solve minimize cost;

output [ "an_cov=", show(an_cov), "\n"
       , "dn_def=", show(dn_def), "\n"
       , "dn_reg=", show(dn_reg), "\n"
       , "sn_def=", show(sn_def), "\n"
       , "pi_sel=", show(pi_sel), "\n"
       , "pi_bb=", show(pi_bb), "\n"
       , "bb_order=", show(bb_order), "\n"
       , "br_bb_dists=", show(br_bb_dists), "\n"
       , "null_label=", show(nullLabel), "\n"
       , "cost=", show(cost), "\n"
       ];
