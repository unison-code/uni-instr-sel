--------------------------------------------------------------------------------
-- |
-- Module      :  Language.InstructionSelection.Program.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
-- 
-- Contains the data and types for representing programs. The computational
-- parts (i.e. the code within the blocks) are represented as DAGs which
-- capture the data flow whilst permitting common subexpression elimination.
-- 
--------------------------------------------------------------------------------

module Language.InstructionSelection.Program.Base where

-- | Record for containing a temporary.

data Temporary
    = Temporary {
          tempId :: Integer
      }
    deriving (Show, Eq)

-- | Record for containing a constant value.

data Const
    = Const {
          constValue :: Integer
      }
    deriving (Show, Eq)

-- | Record for describing a node in the @Computation@ graph.

data ComputationNode

       -- | The @DataNode@ represents a computation value. This value may need,
       -- at a later point in the compilation, to be stored at a data location
       -- such as in a register or in memory. However, it may also be that the
       -- value is computed as an intermediate value as part of a complex
       -- instruction. In such instances the value is located within the
       -- pipeline and thus do not require to be allocated a particular data
       -- location.

     = DataNode {
           -- | TODO: add data
       }

       -- | The @OpNode@ represents an operation which consumes one or more data
       -- values and produces one or more data values.

     | OpNode {
           -- | TODO: add data
       }

       -- | The @TransferNode@ represents a transfer of data located at one
       -- particular location to another. If the data locations belong to
       -- different storage classes, then an operation is necessary to enable
       -- that transfer. In contrast, if the data locations belong to the same
       -- storage class, then no such operation is required. In traditional
       -- compilers this transfer has been treated either through approximation
       -- or as a post-step in code emission (i.e. after instruction selection
       -- and register allocation has been performed), but through the use of
       -- data nodes this transfer becomes apparent already in the instruction
       -- selection phase.

     | TransferNode {
           -- | TODO: add data
       }

     deriving (Show)

-- | Record for describing a computation. The @Computation@ is a directed
-- acyclic graph (DAG) where the nodes represent computations and the edges
-- represent data dependencies between computations. The graph itself is
-- represented using @Data.Graph.Inductive.Tree.Gr@ where the nodes are labeled
-- with @ComputationNode@s and the edges are labeled with
-- @ComputationDescription@s.

data Computation
    = Computation {
      }
    deriving (Show)

-- | Record for describing a block within the @CFG@. The @Block@ consists of a
-- forest of @Computation@s. Two @Computation@s within the same @Block@ are
-- completely independent of each other and can be processed separately without
-- affecting the expected behaviour of the program (provided the @Computation@s
-- do not migrate to another @Block@).

data Block
    = Block {

          dags :: [Computation]

      }
    deriving (Show)

-- | Record for describing a directed control flow graph (CFG). The graph is
-- represented internally using @Data.Graph.Inductive.Tree.Gr@ where the nodes
-- consists of @Block@s and the edges (labeled as @BranchingData@) between the
-- @Block@s denote the possible branchings at runtime.

data CFG 
    = CFG {

          -- | Graph where the nodes are labeled with @Block@ and the edges are
          -- labeled with @BranchingData@.
      
          graph :: Gr Block BranchingData

      }
    deriving (Show)

-- | Record for describing a function. A @Function@ is, like @Module@, a unit
-- which can be compiled on its own. Each @Function@ consists of a control-flow
-- graph (@CFG@) which describes the possible flows of execution for the
-- @Function@.

data Function
    = Function {

          -- | The control-flow graph of this @Function@.

          cfg :: CFG

      }
    deriving (Show)

-- | Record for describing a module. A @Module@ is a self-contained unit which
-- can be compiled on its own. In turn, each @Module@ consists of a set of
-- @Function@s, which also are compiled in isolation.

data Module
    = Module {

          -- | List of @Function@s which comprises the @Module@. The @Module@
          -- must consist of at least one @Function@.

          functions :: [Function]

      }
    deriving (Show)

-- | Record for describing a program. All @Program@s consists of a set of
-- @Module@s, which is a self-contained unit which can be compiled on its own
-- (dependencies on other functions are resolved at linking time, not compile
-- time). This is the outer-most level of data records.

data Program 
    = Program {

          -- | List of @Module@s which comprises the @Program@. The @Program@
          -- must consist of at least one @Module@.

          modules :: [Module]

      }
    deriving (Show)