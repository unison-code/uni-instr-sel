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
-- Contains the data and types for representing programs. The expressions
-- (i.e. the code within the blocks) are represented as DAGs which capture the
-- data flow whilst permitting common subexpression elimination.
-- 
--------------------------------------------------------------------------------

module Language.InstructionSelection.Program.Base where

import Data.Graph.Inductive.Tree

-- | Record for containing a temporary.

data Temporary
    = Temporary {
          tempId :: Integer
      }
    deriving (Show, Eq)

-- | Record for containing a constant value.

data Constant
    = Constant {
          constValue :: Integer
      }
    deriving (Show, Eq)

-- | Record for describing a data value. This is simply a wrapper to allow
-- many different types of values.

data DataValue
    = TemporaryDataValue {
          tempDataValue :: Temporary
      }
    | ConstantDataValue {
          constDataValue :: Constant
      }
    deriving (Show)

-- | Record for describing an operation.

data Operation
    = IntAdd
    | FloatAdd
    deriving (Show)

-- | Record for describing a node in the @Expression@ graph.

data ExpressionNode

       -- | The @DataNode@ represents a data value. This value may need, at a
       -- later point in the compilation, to be stored at a data location such
       -- as in a register or in memory. However, it may also be that the value
       -- is computed as an intermediate value as part of a complex
       -- instruction. In such instances the value is located within the
       -- pipeline and thus do not require to be allocated a particular data
       -- location.

     = DataNode {
           dataValue :: DataValue
       }

       -- | The @OpNode@ represents an operation which consumes one or more data
       -- values and produces one or more data values.

     | OpNode {
           op :: Operation
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

-- | Record for describing additional information about a data
-- dependency. Currently none is needed, but if it should be required in the
-- future then this data type should facilitate that addition.

data DataDependencyInfo
    = DataDependencyInfo
    deriving (Show)

-- | Record for describing an expression. The @Expression@ is a directed acyclic
-- graph (DAG) where the nodes represent operations and the edges represent data
-- dependencies between operations. The graph itself is represented using
-- @Data.Graph.Inductive.Tree.Gr@ where the nodes are labeled with
-- @ExpressionNode@s and the edges are labeled with @DataDependencyInfo@s.

data Expression
    = Expression {
          expGraph :: Gr ExpressionNode DataDependencyInfo
      }
    deriving (Show)

-- | Record for describing a branching edge between two @Block@s. Right now it
-- contains nothing, but if additional edge information turns out to be required
-- in the future, this should facilitate such augmentations.

data BranchingInfo
    = BranchingInfo
    deriving (Show)

-- | Record for describing a block within the @CFG@. The @Block@ consists of a
-- forest of @Expression@s. Two @Expression@s within the same @Block@ are
-- completely independent of each other and can be processed separately without
-- affecting the expected behaviour of the program (provided the @Expression@s
-- do not migrate to another @Block@).

data Block
    = Block {
          dags :: [Expression]
      }
    deriving (Show)

-- | Record for describing a directed control flow graph (CFG). The graph is
-- represented internally using @Data.Graph.Inductive.Tree.Gr@ where the nodes
-- consists of @Block@s and the edges (labeled as @BranchingInfo@) between the
-- @Block@s denote the possible branchings at runtime.

data CFG 
    = CFG {
          cfgGraph :: Gr Block BranchingInfo
      }
    deriving (Show)

-- | Record for describing a function. A @Function@ is, like @Module@, a unit
-- which can be compiled on its own. Each @Function@ consists of a control-flow
-- graph (@CFG@) which describes the possible flows of execution for the
-- @Function@.

data Function
    = Function {
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