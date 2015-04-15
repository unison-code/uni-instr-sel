% Converts *.ll.model.json -->  *.ll.model.ext.json
%
% The idea is to convert all explicit constraints to parameterized form, in a few arrays.
% Canonical example (fact) below.  If "fun-constraints" are non-null, this might break down.
% The conversion target is match-constraints.

/***

{"match-entry-label-nodes": ...,
 "num-locations":35,
 "match-non-copy-instrs": ...,
 "match-code-sizes": ...,
 "fun-num-label-nodes":7,
 "fun-constraints":[],
 "fun-state-nodes":[],
 "match-entity-nodes-used": ...,
 "fun-label-dom-sets": ...,
 "match-adduc-settings": ...,
 "match-entity-nodes-defined": ...,
 "fun-def-edges":[[8,9],[2,4],[],[],[10,13],[],[]],
 "fun-entry-label-node":0,
 "fun-num-op-nodes":24,
 "match-non-entry-label-nodes": ...,
 "match-latencies": ...,
 "fun-bb-exec-freqs":[16384,524288,507904,16384,507904,524288,524288],
 "match-constraints":
   [["(&& (== (loc-to-num (loc-of-dnode (ai 8))) (loc-to-num (loc-of-dnode (ai 13)))) (== (loc-to-num (loc-of-dnode (ai 13))) (loc-to-num (loc-of-dnode (ai 4)))))"],
    ["(&& (== (loc-to-num (loc-of-dnode (ai 9))) (loc-to-num (loc-of-dnode (ai 10)))) (== (loc-to-num (loc-of-dnode (ai 10))) (loc-to-num (loc-of-dnode (ai 2)))))"],
    ["(== (block-to-num (block-of-lnode (ai 6))) (block-to-num (block-of-match (ai 2))))","(fall-through (ai 2) (block-of-lnode (ai 2)))"],
    ["(== (block-to-num (block-of-lnode (ai 5))) (block-to-num (block-of-match (ai 3))))","(fall-through (ai 3) (block-of-lnode (ai 3)))"],
    ["(== (block-to-num (block-of-lnode (ai 4))) (block-to-num (block-of-match (ai 4))))","(fall-through (ai 4) (block-of-lnode (ai 1)))"],
    ["(== (block-to-num (block-of-lnode (ai 2))) (block-to-num (block-of-match (ai 5))))","(fall-through (ai 5) (block-of-lnode (ai 4)))"],
    ["(== (block-to-num (block-of-lnode (ai 0))) (block-to-num (block-of-match (ai 6))))","(fall-through (ai 6) (block-of-lnode (ai 1)))"],
    ["(== (block-to-num (block-of-lnode (ai 0))) (block-to-num (block-of-match (ai 7))))"],
    ["(== (block-to-num (block-of-lnode (ai 0))) (block-to-num (block-of-match (ai 8))))"],
    ["(== (block-to-num (block-of-lnode (ai 0))) (block-to-num (block-of-match (ai 9))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 6))) (loc-to-num (loc-of-dnode (ai 18))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (loc-of-dnode (ai 16))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (loc-of-dnode (ai 15))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (loc-of-dnode (ai 14))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 3))) (loc-to-num (loc-of-dnode (ai 13))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (loc-of-dnode (ai 12))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (loc-of-dnode (ai 11))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (loc-of-dnode (ai 10))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (loc-of-dnode (ai 9))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (loc-of-dnode (ai 8))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))"],
    ["(== (block-to-num (block-of-lnode (ai 1))) (block-to-num (block-of-match (ai 31))))",
     "(== (loc-to-num (loc-of-dnode (ai 6))) (loc-to-num null))",
     "(fall-through (ai 31) (block-of-lnode (ai 5)))"],
    ["(== (block-to-num (block-of-lnode (ai 1))) (block-to-num (block-of-match (ai 32))))",
     "(== (loc-to-num (loc-of-dnode (ai 6))) (loc-to-num null))",
     "(fall-through (ai 32) (block-of-lnode (ai 6)))"],
    ["(== (block-to-num (block-of-lnode (ai 1))) (block-to-num (block-of-match (ai 33))))",
     "(fall-through (ai 33) (block-of-lnode (ai 5)))"],
    ["(== (block-to-num (block-of-lnode (ai 6))) (block-to-num (block-of-match (ai 34))))"],
    ["(== (block-to-num (block-of-lnode (ai 5))) (block-to-num (block-of-match (ai 35))))"],
    ["(== (block-to-num (block-of-lnode (ai 4))) (block-to-num (block-of-match (ai 36))))"],
    ["(== (block-to-num (block-of-lnode (ai 2))) (block-to-num (block-of-match (ai 37))))"],
    ["(== (block-to-num (block-of-lnode (ai 0))) (block-to-num (block-of-match (ai 38))))"],
    ["(== (block-to-num (block-of-lnode (ai 3))) (block-to-num (block-of-match (ai 39))))",
     "(== (loc-to-num (loc-of-dnode (ai 12))) (loc-to-num (ai 31)))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 16))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 3))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 13))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 12))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 10))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 9))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (ai 33)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 8))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 16))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 4))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 3))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 13))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 12))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 2))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 1))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 10))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 9))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(== (loc-to-num (loc-of-dnode (ai 0))) (loc-to-num (ai 34)))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 8))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 4))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 16))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 4))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 15))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 4))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 14))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 3))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 13))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 2))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 12))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 2))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 11))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 1))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 10))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 0))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 9))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 0))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))",
     "(in-set (loc-to-set-elem (loc-of-dnode (ai 8))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 17))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 19))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"],
    ["(in-set (loc-to-set-elem (loc-of-dnode (ai 17))) 
              (loc-class ((ai 0) (ai 1) (ai 2) (ai 3) (ai 4) (ai 5) (ai 6) (ai 7) (ai 8) (ai 9) (ai 10) (ai 11) (ai 12) (ai 13) (ai 14) (ai 15) (ai 16) (ai 17) (ai 18) (ai 19) (ai 20) (ai 21) (ai 22) (ai 23) (ai 24) (ai 25) (ai 26) (ai 27) (ai 28) (ai 29) (ai 30) (ai 31))))"]],
 "match-op-nodes-covered": ...,
 "fun-num-entity-nodes":20,
 "num-matches":71
}

The "match-constraints" element should be replaced by:

 "match-constraints": [[], ..., []],

"same-loc": [[0, 8, 13], [0, 13, 4], [1, 9, 10], [1, 10, 2],
              [10, 6, 18], [11, 4, 16], [12, 4, 15], [13, 4, 14],
	      [14, 3, 13], [15, 2, 12], [16, 2, 11], [17, 1, 10], [18, 0, 9], [19, 0, 8]],

 "in-block-succ": [[2, 6, 2], [3, 5, 3], [4, 4, 1], [5, 2, 4], [6, 0, 1],
                   [31, 1, 5], [32, 1, 6], [33, 1, 5]],

 "in-block": [[7, 0], [8, 0], [9, 0], [34, 6], [35, 5], [36, 4], [37, 2], [38, 0], [39, 3]],

 "loc-domain": [[20, 19, 0, 31], [20, 14, 0, 31], [20, 3, 0, 31],
                   [21, 19, 0, 31], [21, 14, 0, 31], [21, 3, 0, 31],
                   [22, 19, 0, 31], [22, 14, 0, 31], [22, 3, 0, 31],
                   [23, 19, 0, 31], [23, 3, 0, 31],
                   [24, 15, 0, 31], [24, 11, 0, 31], [24, 1, 34, 34],
                   [25, 15, 0, 31], [25, 11, 0, 31], [25, 1, 34, 34],
                   [26, 14, 0, 31], [26, 3, 0, 31], 
                   [27, 15, 0, 31], [27, 11, 0, 31], [27, 1, 34, 34],
                   [28, 15, 0, 31], [28, 11, 0, 31], [28, 1, 34, 34],
                   [29, 15, 0, 31], [29, 11, 0, 31], [29, 1, 34, 34],
                   [30, 15, 0, 31], [30, 11, 0, 31], [30, 1, 34, 34],
                   [31, 6, -1, -1],
                   [32, 6, -1, -1],
                   [39, 12, 31, 31],
		   [40, 4, 33, 33], [40, 16, 0, 31],
		   [41, 4, 33, 33], [41, 15, 0, 31],
		   [42, 4, 33, 33], [42, 14, 0, 31],
		   [43, 3, 33, 33], [43, 13, 0, 31],
		   [44, 2, 33, 33], [44, 12, 0, 31],
		   [45, 2, 33, 33], [45, 11, 0, 31],
		   [46, 1, 33, 33], [46, 10, 0, 31],
		   [47, 0, 33, 33], [47, 9, 0, 31],
		   [48, 0, 33, 33], [48, 8, 0, 31],
		   [49, 4, 34, 34], [49, 16, 0, 31],
		   [50, 4, 34, 34], [50, 15, 0, 31],
		   [51, 4, 34, 34], [51, 14, 0, 31],
		   [52, 3, 34, 34], [52, 13, 0, 31],
		   [53, 2, 34, 34], [53, 12, 0, 31],
		   [54, 2, 34, 34], [54, 11, 0, 31],
		   [55, 1, 34, 34], [55, 10, 0, 31],
		   [56, 0, 34, 34], [56, 9, 0, 31],
		   [57, 0, 34, 34], [57, 8, 0, 31],
		   [58, 4, 0, 31], [58, 16, 0, 31],
		   [59, 4, 0, 31], [59, 15, 0, 31],
		   [60, 4, 0, 31], [60, 14, 0, 31],
		   [61, 3, 0, 31], [61, 13, 0, 31],
		   [62, 2, 0, 31], [62, 12, 0, 31],
		   [63, 2, 0, 31], [63, 11, 0, 31],
		   [64, 1, 0, 31], [64, 10, 0, 31],
		   [65, 0, 0, 31], [65, 9, 0, 31],
		   [66, 0, 0, 31], [66, 8, 0, 31],
		   [67, 19, 0, 31],
		   [68, 17, 0, 31],
		   [69, 19, 0, 31],
		   [70, 17, 0, 31]],
                   
***/

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(avl)).
:- use_module(library(codesio)).

test :-
	see('fact.ll.model.json'),
	tell('fact.ll.model.ext.json'),
	extend,
	told,
	seen.

extend_and_halt :-
	extend, !, halt.
extend_and_halt :-
	halt(-1).

extend :-
	json2avl(AVL1),
	avl_fetch('match-constraints', AVL1, MatchConstraints1),
	convert(MatchConstraints1, MatchConstraints2, SameLoc, InBlockSucc, InBlock, LocDomain),
	avl_store('match-constraints', AVL1, MatchConstraints2, AVL2),
	avl_store('same-loc', AVL2, SameLoc, AVL3),
	avl_store('in-block-succ', AVL3, InBlockSucc, AVL4),
	avl_store('in-block', AVL4, InBlock, AVL5),
	avl_store('loc-domain', AVL5, LocDomain, AVLn),
	avl2json(AVLn).

convert(MC1, MC2, SameLoc, InBlockSucc, InBlock, LocDomain) :-
	(   foreach(Strings,MC1),
	    foreach([],MC2),
	    count(M,0,_),
	    fromto(Ctrs1,Ctrs2,Ctrs5,[])
	do  (   foreach(String,Strings),
		fromto(Ctrs2,Ctrs3,Ctrs4,Ctrs5),
		param(M)
	    do  tokenize(Tokens, String, []),
		parse(Ctr, Tokens, []),
		split(Ctr, M, Ctrs3, Ctrs4)
	    )
	),
	(   foreach(Ct,Ctrs1),
	    foreach(F-Args,KL1)
	do  Ct =.. [F|Args]
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   select(same_loc-SameLoc, KL3, KL4) -> true
	;   SameLoc = [],
	    KL3 = KL4
	),
	(   select(in_block-InBlock0, KL4, KL5) -> true
	;   InBlock0 = [],
	    KL4 = KL5
	),
	(   select(fall_through-FallThru, KL5, KL6) -> true
	;   FallThru = [],
	    KL5 = KL6
	),
	(   select(loc_domain-LocDomain, KL6, _) -> true
	;   LocDomain = []
	),
	(   foreach([Key,S],FallThru),
	    foreach([Key,B,S],InBlockSucc),
	    fromto(InBlock0,InBlock1,InBlock2,InBlock)
	do  selectchk([Key,B], InBlock1, InBlock2)
	).
	

split(&&(X,Y), M) --> !, split(X, M), split(Y, M).
split(('loc-to-num'('loc-of-dnode'(ai(A)))=='loc-to-num'('loc-of-dnode'(ai(B)))), M) --> !, 
	[same_loc(M,A,B)].
split(('block-to-num'('block-of-lnode'(ai(A)))=='block-to-num'('block-of-match'(ai(M)))), M) --> !, 
	[in_block(M,A)].
split('fall-through'(ai(M),'block-of-lnode'(ai(S))), M) --> !,
	[fall_through(M,S)].
split('in-set'('loc-to-set-elem'('loc-of-dnode'(ai(A))),'loc-class'(Range)), M) --> !,
	[loc_domain(M,A,Min,Max)],
	{min_member(ai(Min), Range)},
	{max_member(ai(Max), Range)},
	{length(Range, Len)},
	{Max-Min =:= Len-1}.
split(('loc-to-num'('loc-of-dnode'(ai(A)))=='loc-to-num'(ai(B))), M) --> !,
	[loc_domain(M,A,B,B)].
split(('loc-to-num'('loc-of-dnode'(ai(A)))=='loc-to-num'(null)), M) --> !,
	[loc_domain(M,A,locValueForNull,locValueForNull)].

parse(Term) --> ['('], !, parse_args([F|Args]),
	{atomic(F) -> Term =.. [F|Args] ; Term = [F|Args]}.
parse(Term) --> [Term].

parse_args([]) --> [')'], !.
parse_args([Arg|Args]) --> parse(Arg), parse_args(Args).

tokenize(Tokens) --> " ", !,
	tokenize(Tokens).
tokenize(['('|Tokens]) --> "(", !,
	tokenize(Tokens).
tokenize([')'|Tokens]) --> ")", !,
	tokenize(Tokens).
tokenize([Token|Tokens]) --> [C], !,
	token([C|Tail], Tail, Token),
	tokenize(Tokens).
tokenize([]) --> [].

token(Codes, [C|Tail], Token) --> [C], {nonmember(C, " ()")}, !,
	token(Codes, Tail, Token).
token(Codes, [], Token) --> {name(Token, Codes)}.

json2avl(AVL) :-
	read_line(Line1),
	(   fromto(Line1,Line2,Line3,end_of_file),
	    fromto(Lines1,[[10],Line2|Lines2],Lines2,[".\n"])
	do  read_line(Line3)
	),
	append(Lines1, LongLine),
	read_from_codes(LongLine, {Term}),
	term_to_keylist(Term, KL1, []),
	keysort(KL1, KL2),
	ord_list_to_avl(KL2, AVL).
	    
avl2json(AVL) :-
	avl_to_list(AVL, KL1),
	length(KL1, Len),
	(   foreach(Key-Val,KL1),
	    count(I,1,_),
	    param(Len)
	do  (I>1 -> true ; write('{\n')),
	    (I<Len -> Sep = ',' ; Sep = '}'),
	    format(' "~w": ', [Key]),
	    (   string_key(Key) ->
		write_string_list(Val)
	    ;   write(Val)
	    ),
	    write(Sep), nl
	).

string_key(artificial_string_key).

write_string_list(Val) :-
	(   foreach(V,Val),
	    fromto('[',Sep,', ',_)
	do  format('~w"~s"', [Sep,V])
	),
	write(']').

term_to_keylist((T1,T2)) --> !,
	term_to_keylist(T1),
	term_to_keylist(T2).
term_to_keylist(Qname:T) --> [Name-T],
	{atom_codes(Name, Qname)}.
