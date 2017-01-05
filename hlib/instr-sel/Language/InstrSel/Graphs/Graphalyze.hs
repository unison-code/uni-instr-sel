{-|
Copyright  :  Copyright (c) 2008, 2012-2017,
              Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>,
              Gabriel Hjort Blindell <ghb@kth.se>
License    :  BSD3 (see the LICENSE file)
Maintainer :  ghb@kth.se

Mainly functions copied from the Graphalyze package, but also includes other,
useful functions.
-}
{-
Main authors:
  Ivan Lazar Miljenovic <Ivan.Miljenovic@gmail.com>
  Gabriel Hjort Blindell <ghb@kth.se>

-}

module Language.InstrSel.Graphs.Graphalyze
  ( areGraphsIsomorphic
  , cyclesIn
  , cyclesIn'
  , isReachableComponent
  , isReachableComponent'
  , strongComponentsOf
  , strongComponentsOf'
  , weakComponentsOf
  , weakComponentsOf'
  )
where

import Language.InstrSel.Graphs.Base
import Language.InstrSel.Graphs.PatternMatching.VF2

import qualified Data.Graph.Inductive as I

import qualified Data.Map as M

import Control.Arrow
  ( first )
import Data.List
  ( unfoldr
  , foldl'
  )
import Data.Maybe
  ( fromJust )



--------------
-- Data types
--------------

-- | Contains the necessary data structures used by 'cyclesIn'.
data CyclesInState g a b
  = CyclesInState
      { cisCycles :: [[I.Node]]
        -- ^ The cycles found so far, in topological order.
      , cisBlocked :: M.Map I.Node Bool
        -- ^ The nodes which are currently blocked.
      , cisBlockMap :: M.Map I.Node [I.Node]
        -- ^ The B set.
      , cisStack :: [I.Node]
        -- ^ The node stack.
      , cisS :: Maybe I.Node
        -- ^ The current S value.
      , cisCurrentComp :: Maybe (g a b)
        -- ^ The component currently being processed.
      , cisComponents :: [g a b]
        -- ^ The components of the input graph.
      , cisGraph :: g a b
        -- ^ The input graph.
      }
  deriving (Show)

-- | Contains the necessary data structures used by 'strongComponentsOf'.
data SCCState g a b
  = SCCState
      { sccComponents :: [g a b]
        -- ^ The components found so far.
      , sccCurrentIndex :: Int
        -- ^ The current index.
      , sccStack :: [I.Node]
        -- ^ The node stack.
      , sccNodeInfo :: M.Map I.Node (Bool, Int, Int)
        -- ^ Node information as a tuple (whether the node is on the stack, its
        -- index, and its low link).
      , sccGraph :: g a b
        -- ^ The input graph.
      }
  deriving (Show)



-------------
-- Functions
-------------

-- | Find all weakly connected components of a graph.
weakComponentsOf :: Graph -> [Graph]
weakComponentsOf g =
  map ( \int_g -> let ns = map Node $ I.labNodes int_g
                  in Graph { intGraph = int_g
                           , intNodeMap = M.fromList $ groupNodesByID ns
                           }
      ) $
  weakComponentsOf' (intGraph g)

-- | Find all weakly connected components of a graph.
weakComponentsOf' :: (I.DynGraph g) => g a b -> [g a b]
weakComponentsOf' = unfoldr splitComponent

-- | Find the next component and split it off from the graph.
splitComponent :: (I.DynGraph g) => g a b -> Maybe (g a b, g a b)
splitComponent g
    | I.isEmpty g = Nothing
    | otherwise = Just .            -- Get the type right
                  first I.buildGr . -- Create the subgraph
                  extractNode .     -- Extract components of subgraph
                  first Just .      -- Getting the types right
                  I.matchAny $ g    -- Choose an arbitrary node to begin with

-- | Extract the given node and all nodes it is transitively
--   connected to from the graph.
extractNode :: (I.DynGraph g) => I.Decomp g a b -> ([I.Context a b], g a b)
extractNode (Nothing,gr) = ([],gr)
extractNode (Just ctxt, gr)
    | I.isEmpty gr = ([ctxt], I.empty)
    | otherwise  = first (ctxt:) $ foldl' nodeExtractor ([],gr) nbrs
    where
    nbrs = I.neighbors' ctxt

-- | Helper function for 'extractNode' above.
nodeExtractor :: (I.DynGraph g) => ([I.Context a b], g a b) -> I.Node
              -> ([I.Context a b], g a b)
nodeExtractor cg@(cs,g) n
    | I.gelem n g = first (++ cs) . extractNode $ I.match n g
    | otherwise = cg

-- | Tests whether there is a path in the graph @g@ from a node in component
-- @c1@ to a node in component @c2@.
isReachableComponent
  :: Graph
     -- ^ Graph @g@.
  -> Graph
     -- ^ Component @c1@.
  -> Graph
     -- ^ Component @c2@.
  -> Bool
isReachableComponent g c1 c2 =
  isReachableComponent' (intGraph g) (intGraph c1) (intGraph c2)

-- | Tests whether there is a path in the graph @g@ from a node in component
-- @c1@ to a node in component @c2@.
isReachableComponent'
  :: (I.DynGraph g)
  => g a b
     -- ^ Graph @g@.
  -> g a b
     -- ^ Component @c1@.
  -> g a b
     -- ^ Component @c2@.
  -> Bool
isReachableComponent' g c1 c2 =
  or [ isReachable g n1 n2 | n1 <- I.nodes c1
                           , n2 <- I.nodes c2
                           , n1 /= n2
     ]
  where
  isReachable g' n1 n2 = n2 `elem` (I.reachable n1 g')

-- | Tests if two graphs are isomorphic to each other.
areGraphsIsomorphic :: Graph -> Graph -> Bool
areGraphsIsomorphic g1 g2 = length (findMatches g1 g2) > 0

-- | Finds all cycles in a given graph.
cyclesIn :: Graph -> [[Node]]
cyclesIn g =
  let int_g = intGraph g
      cs = cyclesIn' int_g
  in map (map (\i -> Node (i, fromJust $ I.lab int_g i))) cs

-- | Finds all cycles in a given graph using Johnson's algorithm. The first and
-- last elements in an inner list is always the same element.
--
-- See Donald B. Johnson: Finding All the Elementary Circuits of a Directed
-- Graph. SIAM Journal on Computing. Volumne 4, Nr. 1 (1975), pp. 77-84.
cyclesIn' :: (I.DynGraph g) => g a b -> [[I.Node]]
cyclesIn' g =
  cisCycles $
  foldr cyclesFor (mkInitCyclesInState g) (I.nodes g)

cyclesFor
  :: (I.DynGraph g)
  => I.Node
  -> CyclesInState g a b
  -> CyclesInState g a b
cyclesFor n st0 =
  let n_comp = head $
               filter (\c -> n `I.gelem` c) $
               cisComponents st0
  in if I.noNodes n_comp > 1
     then let st1 = st0 { cisS = Just n
                        , cisCurrentComp = Just n_comp
                        }
              st2 = fst $ cCircuits n st1
              g = cisGraph st2
              new_g = I.delNode n g
              new_comps = strongComponentsOf' new_g
              st3 = st2 { cisGraph = new_g
                        , cisComponents = new_comps
                        }
          in st3
     else st0 -- Skip to next node

cCircuits
  :: (I.DynGraph g)
  => I.Node
  -> CyclesInState g a b
  -> (CyclesInState g a b, Bool)
cCircuits n st0 =
  let st1 = st0 { cisBlocked = M.insert n True (cisBlocked st0)
                , cisStack = (n:cisStack st0)
                }
      c = fromJust $ cisCurrentComp st1
      n_suc = I.suc c n
      (st2, f) =
        foldr ( \m (st, f') ->
                if m == fromJust (cisS st)
                then let new_cycle = reverse (m:cisStack st)
                         st' = st { cisCycles = (new_cycle:cisCycles st) }
                     in (st', True)
                else if not (cisBlocked st M.! m)
                     then let (st', f'') = cCircuits m st
                          in (st', f' || f'')
                     else (st, f')
              )
              (st1, False)
              n_suc
      st3 = if f
            then cUnblock n st2
            else foldr ( \m st ->
                         let bm = cisBlockMap st
                             m_blocked = bm M.! m
                             new_m_blocked = (n:m_blocked)
                         in if n `notElem` m_blocked
                            then st { cisBlockMap =
                                        M.insert m new_m_blocked bm
                                    }
                            else st
                       )
                       st2
                       n_suc
      st4 = st3 { cisStack = tail $ cisStack st3 }
  in (st4, f)

cUnblock
  :: (I.DynGraph g)
  => I.Node
  -> CyclesInState g a b
  -> CyclesInState g a b
cUnblock n st0 =
  let n_blocked = cisBlockMap st0 M.! n
      st1 = st0 { cisBlocked = M.insert n False (cisBlocked st0)
                , cisBlockMap = M.insert n [] (cisBlockMap st0)
                }
      st2 = foldr ( \m st ->
                    if cisBlocked st M.! m
                    then cUnblock m st
                    else st
                  )
                  st1
                  n_blocked
  in st2

mkInitCyclesInState :: (I.DynGraph g) => g a b -> CyclesInState g a b
mkInitCyclesInState g =
  let ns = I.nodes g
  in CyclesInState { cisCycles = []
                   , cisBlocked = M.fromList $ zip ns (repeat False)
                   , cisBlockMap = M.fromList $ zip ns (repeat [])
                   , cisStack = []
                   , cisS = Nothing
                   , cisCurrentComp = Nothing
                   , cisComponents = strongComponentsOf' g
                   , cisGraph = g
                   }

-- | Find all strongly connected components of a graph.
strongComponentsOf :: Graph -> [Graph]
strongComponentsOf g =
  map ( \int_g -> let ns = map Node $ I.labNodes int_g
                  in Graph { intGraph = int_g
                           , intNodeMap = M.fromList $ groupNodesByID ns
                           }
      ) $
  strongComponentsOf' (intGraph g)

-- | Find all strongly connected components of a graph. Implements Tarjan's
-- algorithm. Returned list is sorted in topological order.
strongComponentsOf' :: (I.DynGraph g) => g a b -> [g a b]
strongComponentsOf' g =
  sccComponents $
  foldr ( \n st ->
          let (_, i, _) = sccNodeInfo st M.! n
          in if i < 0 then findSCCFor n st else st
        )
        (mkInitSCCState g)
        (I.nodes g)

findSCCFor
  :: (I.DynGraph g)
  => I.Node
  -> SCCState g a b
  -> SCCState g a b
findSCCFor n st0 =
  let i = sccCurrentIndex st0
      st1 = st0 { sccCurrentIndex = i + 1
                , sccStack = (n:sccStack st0)
                , sccNodeInfo = M.insert n (True, i, i) (sccNodeInfo st0)
                }
      g = sccGraph st1
      st2 = foldr ( \m st ->
                    let st_ni = sccNodeInfo st
                        (m_on_stack, m_index, _) = st_ni M.! m
                    in if m_index < 0
                       then let st' = findSCCFor m st
                                st_ni' = sccNodeInfo st'
                                (n_on_stack', n_index', n_lowlink') =
                                  st_ni' M.! n
                                (_, _, m_lowlink) = st_ni' M.! m
                                new_n_ni = ( n_on_stack'
                                           , n_index'
                                           , min n_lowlink' m_lowlink
                                           )
                            in st' { sccNodeInfo =
                                       M.insert n new_n_ni st_ni'
                                   }
                       else if m_on_stack
                            then let (n_on_stack', n_index', n_lowlink') =
                                       st_ni M.! n
                                     new_n_ni = ( n_on_stack'
                                                , n_index'
                                                , min n_lowlink' m_index
                                                )
                                 in st { sccNodeInfo =
                                           M.insert n new_n_ni st_ni
                                       }
                            else st
                  )
                  st1
                  (I.suc g n)
      (_, n_index, n_lowlink) = sccNodeInfo st2 M.! n
      st3 = if n_index == n_lowlink
            then let stack = sccStack st2
                     (p0, p1) = span (/= n) stack
                     comp_ns = (head p1:p0)
                     new_stack = tail p1
                     new_ni = foldr ( \n' ni ->
                                      let (_, n_index', n_lowlink') = ni M.! n'
                                          new_n_ni = ( False
                                                     , n_index'
                                                     , n_lowlink'
                                                     )
                                      in M.insert n' new_n_ni ni
                                    )
                                    (sccNodeInfo st2)
                                    comp_ns
                     comp = I.nfilter (`elem` comp_ns) (sccGraph st2)
                     new_cs = (comp:sccComponents st2)
                 in st2 { sccComponents = new_cs
                        , sccStack = new_stack
                        , sccNodeInfo = new_ni
                        }
            else st2
  in st3

mkInitSCCState :: (I.DynGraph g) => g a b -> SCCState g a b
mkInitSCCState g =
  let ns = I.nodes g
  in SCCState { sccComponents = []
              , sccCurrentIndex = 0
              , sccStack = []
              , sccNodeInfo = M.fromList $ zip ns (repeat (False, -1, -1))
              , sccGraph = g
              }
