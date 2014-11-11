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

% Pattern graph
int: numNodesInP;
int: numEdgesInP;
array[allEdgesInP, 0..1] of int: edgesInP;

% Function graph
int: numNodesInF;
int: numEdgesInF;
array[allEdgesInF, 0..1] of int: edgesInF;



include "params.mzn";



%=====================
% INTERNAL PARAMETERS
%=====================

set of int: allNodesInP = 0..numNodesInP-1;
set of int: allEdgesInP = 0..numEdgesInP-1;
set of int: allNodesInF = 0..numNodesInF-1;
set of int: allEdgesInF = 0..numEdgesInF-1;



%===========
% VARIABLES
%===========

% Node mappings
array[allNodesInP] of var allNodesInF: NP;

% Edge mappings
array[allEdgesInP] of var allEdgesInF: EP;



%=============
% CONSTRAINTS
%=============

include "alldifferent.mzn";

% All edge mappings must be pair-wise distinct
constraint
  alldifferent(EP);

% All edge mappings must have a corresponding head node mapping
constraint
  forall (n in allNodesInP)
  (
    forall (e in allEdgesInP where edgesInP[e, 0] == n)
    (
      forall (m in allNodesInF)
      (
        NP[n] == m
        <->
        EP[e] in { f | f in allEdgesInF where edgesInF[f, 0] == m }
      )
    )
  );

% All edge mappings must have a corresponding tail node mapping
constraint
  forall (n in allNodesInP)
  (
    forall (e in allEdgesInP where edgesInP[e, 1] == n)
    (
      forall (m in allNodesInF)
      (
        NP[n] == m
        <->
        EP[e] in { f | f in allEdgesInF where edgesInF[f, 1] == m }
      )
    )
  );



%==================
% SOLVE AND OUTPUT
%==================

solve satisfy;

output [ "NP=", show(NP), "\n"
       , "EP=", show(EP), "\n"
       ];
