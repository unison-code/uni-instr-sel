%
%  Main authors:
%    Gabriel Hjort Blindell <ghb@kth.se>
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
array[allOperationsInFunction] of int: coverCostOfOperationInFunction;
int: execFrequencyGCD;

% Match data.
int: numMatches;
array[allMatches] of set of int: operationsCoveredByMatch;
array[allMatches] of int: latencyOfMatch;

% These variables will be set by concatenating the definitions to the end of
% this file.



%=====================
% INTERNAL PARAMETERS
%=====================

% Sets to be used as array ranges.
set of int: allOperationsInFunction = 1..numOperationsInFunction;
set of int: allMatches = 1..numMatches;

% The set of matches that can cover a particular operation.
array[allOperationsInFunction] of set of allMatches: matchsetOfOp =
  [ { m | m in allMatches where op in operationsCoveredByMatch[m] }
  | op in allOperationsInFunction
  ];

% The total cost is computed as:
%
%   sum (m in allMatches)
%   ( latencyOfMatch[m] * execFrequencyOfBlocks[place[m]] )
%
% but this implementation yields poor propagation. Plus, we do not really
% consider block placements in this model; instead we assume a match is placed
% in the block with the highest lowest cover cost of the covered operations.
% Consequently, we use the cost per operation instead of cost per selected
% match.
%
% First, we split the cost incurred by selecting a given match over the
% operations that it covers. As the latency of a match may not be evenly
% divisible by the number of operations covered, some operations may have a
% higher cost than others. This information is put in a matrix called
% 'costPerOpMatrix'.

% The 'costPerOpMatrix' is a 2D matrix with 3 columns:
%    col 1: an operation o
%        2: a match m that covers o
%        3: the cost incurred by o if m is selected
% For simplicity, we first create a list and then the matrix using the list.
array[int] of int: costPerOpList =
  [ if      k = 1 then o
    else if k = 2 then m
    else let
         { int: cover_cost =
             max( [ coverCostOfOperationInFunction[p]
                  | p in operationsCoveredByMatch[m]
                  ]
                )
         , int: prod = latencyOfMatch[m] * cover_cost
         , int: d = card(operationsCoveredByMatch[m])
         , int: q = prod div d
         , int: r = prod mod d
         } in
         if operationsCoveredByMatch[m][r+1] > o then q+1 else q endif
    endif
    endif
  | o in allOperationsInFunction
  , m in allMatches
  , k in 1..3
    where o in operationsCoveredByMatch[m]
  ];
int: numCostPerOpMatrixRows = card(index_set(costPerOpList)) div 3;
array[1..numCostPerOpMatrixRows, 1..3] of int: costPerOpMatrix =
  array2d(1..numCostPerOpMatrixRows, 1..3, costPerOpList);

% A set with all the possible costs that can be incurred by any operation.
set of int: allOpCosts = { costPerOpMatrix[i, 3]
                         | i in 1..numCostPerOpMatrixRows
                         };



%===========
% VARIABLES
%===========

% Match selection.
array[allMatches] of var bool: sel;

% The match that covers a particular operation.
array[allOperationsInFunction] of var allMatches: omatch;

% Cost.
var int: totalcost;
array[allOperationsInFunction] of var allOpCosts: opcosts;



%====================
% GLOBAL CONSTRAINTS
%====================

include "globals.mzn";



%==================
% BASE CONSTRAINTS
%==================

% For each operation o, exactly one match must be selected such that o is
% covered.
constraint
  forall (o in allOperationsInFunction)
  ( omatch[o] in matchsetOfOp[o]
    /\
    forall (m in matchsetOfOp[o])
    ( omatch[o] = m <-> sel[m] )
  );

% Constrain the cost that can be incurred by each operation.
constraint
  forall (op in allOperationsInFunction)
  ( table([op, omatch[op], opcosts[op]], costPerOpMatrix) );

% The total cost is the sum of the costs incurred by all operations.
constraint
  totalcost = sum(opcosts);



%==================
% SOLVE AND OUTPUT
%==================

solve ::
  int_search(opcosts, largest, indomain_min, complete)
  minimize totalcost;

% oplace, omatch, dmatch are handy for debugging
output [ "sel=", show(sel), "\n"
       , "cost=", show(execFrequencyGCD * totalcost), "\n"
       ];



% Local Variables:
% tab-width: 8
% End:
