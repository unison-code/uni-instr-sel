% PARAMETERS

int: numFuncActionNodes;
int: numFuncEntityNodes;
int: numFuncLabelNodes;
int: numPatternInstances;
array[0..numFuncLabelNodes-1] of set of int: funcLabelDomsets;
array[0..numPatternInstances-1] of int: patInstCosts;
array[0..numPatternInstances-1] of set of int: patInstActionsCovered;
array[0..numPatternInstances-1] of set of int: patInstEntitiesDefined;
array[0..numPatternInstances-1] of set of int: patInstEntitiesUsed;

include "input.mzn";



% VARIABLES

var 0..1: pi_sel;
array[0..numFuncActionNodes-1] of var 0..numPatternInstances-1: an_cov;
array[0..numFuncEntityNodes-1] of var 0..numPatternInstances-1: en_def;

% TODO: add missing variables



% CONSTRAINTS

% TODO: add missing constraints



solve satisfy;
