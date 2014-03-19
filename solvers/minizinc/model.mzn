%------------
% PARAMETERS
%------------

int: numFuncActionNodes;
int: numFuncEntityNodes;
int: numFuncLabelNodes;
int: numPatternInstances;
set of int: allFuncActionNodes = 0..numFuncActionNodes-1;
set of int: allFuncEntityNodes = 0..numFuncEntityNodes-1;
set of int: allFuncLabelNodes = 0..numFuncLabelNodes-1;
set of int: allPatternInstances = 0..numPatternInstances-1;
array[allFuncLabelNodes] of set of int: funcLabelDomsets;
array[allPatternInstances] of int: patInstCosts;
array[allPatternInstances] of set of int: patInstActionsCovered;
array[allPatternInstances] of set of int: patInstEntitiesDefined;
array[allPatternInstances] of set of int: patInstEntitiesUsed;

include "input.mzn";



%-----------
% VARIABLES
%-----------

array[allPatternInstances] of var 0..1: pi_sel;
array[allFuncActionNodes] of var allPatternInstances: an_cov;
array[allFuncEntityNodes] of var allPatternInstances: en_def;
var int: cost;

% TODO: add missing variables



%-------------
% CONSTRAINTS
%-------------

% Prevent a pattern from covering action nodes or defining entity nodes outside
% its range
constraint
forall (p in allPatternInstances)
(
    forall (n in allFuncActionNodes where not (n in patInstActionsCovered[p]))
    (
        an_cov[n] != p
    )
    /\
    forall (n in allFuncEntityNodes where not (n in patInstEntitiesDefined[p]))
    (
        en_def[n] != p
    )
);

% Enforce selection of pattern if an action node is covered or an entity node is
% defined by that pattern
constraint
forall (n in allFuncActionNodes, p in allPatternInstances)
(
    an_cov[n] == p -> pi_sel[p] = 1
)
/\
forall (n in allFuncEntityNodes, p in allPatternInstances)
(
    en_def[n] == p -> pi_sel[p] = 1
);

% Enforce corresponding action node coverage and entity node definitions if a
% pattern is selected
constraint
forall (p in allPatternInstances)
(
    forall (n in patInstActionsCovered[p])
    (
        pi_sel[p] == 1 -> an_cov[n] = p
    )
    /\
    forall (n in patInstEntitiesDefined[p])
    (
        pi_sel[p] == 1 -> en_def[n] = p
    )
);

% TODO: add missing constraints

% Accumulate cost of selected patterns
constraint
cost = sum (p in allPatternInstances)
       (
           patInstCosts[p] * pi_sel[p]
       );



%------------------
% SOLVE AND OUTPUT
%------------------

solve minimize cost;

output [
    "pi_sel=", show(pi_sel), "\n",
    "an_cov=", show(an_cov), "\n",
    "en_def=", show(en_def), "\n",
    "cost=", show(cost), "\n"
];
