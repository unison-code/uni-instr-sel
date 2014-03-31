%------------
% PARAMETERS
%------------

int: numFuncActionNodes;
int: numFuncDataNodes;
int: numFuncStateNodes;
int: numFuncLabelNodes;
int: numPatternInstances;
int: numRegisters;
set of int: allFuncActionNodes = 0..numFuncActionNodes-1;
set of int: allFuncDataNodes = 0..numFuncDataNodes-1;
set of int: allFuncStateNodes = 0..numFuncStateNodes-1;
set of int: allFuncLabelNodes = 0..numFuncLabelNodes-1;
set of int: allFuncLabelNodesPlusNull = 0..numFuncLabelNodes;
set of int: allPatternInstances = 0..numPatternInstances-1;
set of int: allRegisters = 0..numRegisters-1;
int: nullLabel = numFuncLabelNodes;
array[allFuncLabelNodes] of set of int: funcLabelDomsets;
array[allPatternInstances] of bool: patInstNoUseDefDomConstraints;
array[allPatternInstances] of int: patInstCodeSizes;
array[allPatternInstances] of int: patInstLatencies;
array[allPatternInstances] of set of int: patInstActionsCovered;
array[allPatternInstances] of set of int: patInstDataDefined;
array[allPatternInstances] of set of int: patInstDataUsed;
array[allPatternInstances] of set of int: patInstStateDefined;
array[allPatternInstances] of set of int: patInstStateUsed;

include "input.mzn";



%-----------
% VARIABLES
%-----------

array[allFuncActionNodes] of var allPatternInstances: an_cov;
array[allFuncDataNodes] of var allPatternInstances: dn_def;
array[allFuncDataNodes] of var allRegisters: dn_reg;
array[allFuncStateNodes] of var allPatternInstances: sn_def;
array[allPatternInstances] of var bool: pi_sel;
array[allPatternInstances] of var allFuncLabelNodesPlusNull: pi_bb;
var int: cost;



%-------------
% CONSTRAINTS
%-------------

% Prevent a pattern from covering action nodes or defining data or state nodes
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

% Accumulate cost of selected patterns.
% TODO: make this part generic
constraint
cost = sum (p in allPatternInstances)
       (
           patInstCodeSizes[p] * bool2int(pi_sel[p])
       );



%------------------
% SOLVE AND OUTPUT
%------------------

solve minimize cost;

output [
    "an_cov=", show(an_cov), "\n",
    "dn_def=", show(dn_def), "\n",
    "dn_reg=", show(dn_reg), "\n",
    "sn_def=", show(sn_def), "\n",
    "pi_sel=", show(pi_sel), "\n",
    "pi_bb=", show(pi_bb), "\n",
    "cost=", show(cost), "\n"
];
