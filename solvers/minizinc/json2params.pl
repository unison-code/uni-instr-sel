:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(avl)).
:- use_module(library(codesio)).

test :-
	see('fact.ll.model.ext.json'),
	tell('params.mzn'),
	convert,
	told,
	seen.

convert_and_halt :-
	convert, !, halt.
convert_and_halt :-
	halt(-1).

convert :-
	json2avl(AVL),
	avl_to_list(AVL, KL),
	(   foreach(Key-Val,KL)
	do  classify(Key, Class, Name),
	    convert(Class, Name, Val)
	).

convert(void, _, _) :- !.
convert(asis, Name, Value) :- !,
	format('~w = ~w;\n\n', [Name,Value]).
convert(set, Name, Value) :- !,
	asis_to_set(Value, Set),
	format('~w = ~w;\n\n', [Name,Set]).
convert(array1d(IndexSet,asis), Name, Value) :- !,
	format('~w = array1d(~w, ~w);\n\n', [Name,IndexSet,Value]).
convert(array1d(IndexSet,set), Name, List) :- !,
	list_to_sets(List, Sets),
	format('~w = array1d(~w, ~w);\n\n', [Name,IndexSet,Sets]).
convert(array1d(IndexSet,singleton), Name, List) :- !,
	list_to_singletons(List, Sings),
	format('~w = array1d(~w, ~w);\n\n', [Name,IndexSet,Sings]).
convert(array2d(asis), Name, Value) :- !,
	write(Name),
	write(' = ['),
	(   foreach(Row,Value)
	do  write('|'),
	    (   foreach(Elt,Row)
	    do  write(Elt),
		write(', ')
	    )
	),
	write('|];\n\n').

asis_to_set([], {}) :- !.
asis_to_set(List, {Tree}) :- !,
	asis_to_tree(List, Tree).

asis_to_tree([X], X) :- !.
asis_to_tree([X|Xs], (X,Ys)) :-
	asis_to_tree(Xs, Ys).

list_to_sets(Lists, Sets) :-
	(   foreach(L,Lists),
	    foreach(S,Sets)
	do  asis_to_set(L, S)
	).

list_to_singletons(List, Singletons) :-
	(   foreach(L,List),
	    foreach(S,Singletons)
	do  (L=null -> S={} ; S={L})
	).

classify('fun-bb-exec-freqs', array1d(allLabelsInFunction,asis), execFrequencyOfLabelInFunction).
classify('fun-constraints', void, void).
classify('fun-def-edges', array1d(allLabelsInFunction,set), defEdgesForLabelInFunction).
classify('fun-entry-label-node', asis, entryLabelOfFunction).
classify('fun-label-dom-sets', array1d(allLabelsInFunction,set), domSetOfLabelInFunction).
classify('fun-num-entity-nodes', asis, numEntitiesInFunction).
classify('fun-num-label-nodes', asis, numLabelsInFunction).
classify('fun-num-op-nodes', asis, numOperationsInFunction).
classify('fun-state-nodes', asis, stateEntitiesInFunction).
classify('in-block', array2d(asis), inBlock).
classify('in-block-succ', array2d(asis), inBlockSucc).
classify('loc-domain', array2d(asis), locDomain).
classify('match-adduc-settings', array1d(allMatches,asis), applyDefDomUseConstraintForMatch).
classify('match-code-sizes', array1d(allMatches,asis), codeSizeOfMatch).
classify('match-constraints', void, void).
classify('match-entity-nodes-defined', array1d(allMatches,set), entitiesDefinedByMatch).
classify('match-entity-nodes-used', array1d(allMatches,set), entitiesUsedByMatch).
classify('match-entry-label-nodes', array1d(allMatches,singleton), entryLabelOfMatch).
classify('match-latencies', array1d(allMatches,asis), latencyOfMatch).
classify('match-non-copy-instrs', set, nonCopyMatches).
classify('match-non-entry-label-nodes', array1d(allMatches,set), nonEntryLabelsInMatch).
classify('match-op-nodes-covered', array1d(allMatches,set), operationsCoveredByMatch).
classify('num-locations', asis, numLocations).
classify('num-matches', asis, numMatches).
classify('same-loc', array2d(asis), sameLoc).



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

term_to_keylist((T1,T2)) --> !,
	term_to_keylist(T1),
	term_to_keylist(T2).
term_to_keylist(Qname:T) --> [Name-T],
	{atom_codes(Name, Qname)}.
