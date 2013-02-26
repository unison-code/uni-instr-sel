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

import Language.InstructionSelection.Constraints
import Language.InstructionSelection.Misc
import Data.Graph.Inductive.Tree



------------------------------------------------------------
-- Data elements
------------------------------------------------------------

-- | Record for describing data. The data can be of many different types, e.g. a
-- constant, an immediate, a temporary, etc.

data Data

    = Constant {
          constValue :: Integer
      }

    | Immediate {
          immId :: String
      }

    | Temporary {
          tempId :: String
      }

    deriving (Show, Eq)



------------------------------------------------------------
-- Operation elements
------------------------------------------------------------

-- | Record for describing a pure data operation that consumes and/or produces
-- data. Pure operations are those that do not depend on any external state,
-- meaning that they will always produce the same output for the same set of
-- inputs. The consequence of this is that such operations can be ordered in any
-- arbitrary way with respect to other operations. Hence, pure operations are
-- the opposite of side-effect operations.
--
-- For comparison, see @SideEffectOperation@.

data PureOperation

    ------------------------------------------------------------
    -- Arithmetic and logical operations
    ------------------------------------------------------------

    ------------------------------
    -- Integer operations
    ------------------------------

    -- | Integer addition.

    = IAdd

    -- | Integer subtraction.

    | ISub

    -- | Integer multiplication.

    | IMul

    -- | Unsigned integer division.

    | IUDiv

    -- | Signed integer division.

    | ISDiv

    -- | Unsigned integer remainder.

    | IURem
      
    -- | Signed integer remainder.

    | ISRem

    ------------------------------
    -- Floating-point operations
    ------------------------------

    -- | Float addition.

    | FAdd

    -- | Float subtraction.

    | FSub

    -- | Float multiplication.

    | FMul

    -- | Float division.

    | FDiv

    -- | Float remainder.

    | FRem
      
    ------------------------------
    -- Bit operations
    ------------------------------

    -- | Bitwise left shift.

    | Shl

    -- | Bitwise logical right shift.

    | LShr
      
    -- | Bitwise arithmetic right shift (with sign extension).

    | AShr

    -- | Bitwise AND (&).

    | And
      
    -- | Bitwise OR (|).

    | Or

    -- | Bitwise XOR (^).

    | Xor

    ------------------------------------------------------------
    -- Comparison operations
    ------------------------------------------------------------

    -- | Integer comparison.

    | ICmp
      
    -- | Float comparison.

    | FCmp

    ------------------------------------------------------------
    -- Other operations
    ------------------------------------------------------------

    -- | TODO  

    | Phi
    
    deriving (Show)
             
-- | Record for describing a side-effect data operation that consumes and/or
-- produces data. Side-effect operations are those that depend on or modify some
-- external state, meaning that they can produce different results even if the
-- set of inputs is the same. Consequently the execution order in which
-- side-effect operations are arranged with respect to other side-effect
-- operations must be maintained. Hence, side-effect operations are the opposite
-- of pure operations.
--
-- To enforce this execution order, all side-effect operations consume a state
-- node and produce a state node. If a side-effect operation A consumes the
-- state produce by another side-effect operation B, then B must be scheduled
-- before A.
--
-- The edge from the state node to the side-effect operation node must always
-- have input number 0 (i.e. it is always the first edge into the side-effect
-- operation node).
--
-- For comparison, see @PureOperation@.

data SideEffectOperation

    ------------------------------------------------------------
    -- Memory operations
    ------------------------------------------------------------

    -- | Memory load.

    = MemLoad
      
    -- | Memory store.

    | MemStore
    
    deriving (Show)

-- | Record for an operation. An operation is either pure or has side effects.

data Operation
    = POperation {
          pureOp :: PureOperation
      }
    | SEOperation {
          sideEfOp :: SideEffectOperation
    }
    deriving (Show)



------------------------------------------------------------
-- Control and data flow elements
------------------------------------------------------------

-- | Record for describing the node type.

data NodeType

      -- | The @DataNode@ represents nodes which denote data. The data may need,
      -- at a later point in the compilation, to be stored at a data location
      -- such as in a register or in memory. However, it may also be that the
      -- value is computed as an intermediate value as part of a complex
      -- instruction. In such instances the value is located within the pipeline
      -- and thus do not require to be allocated a particular data
      -- location. This depends of course on which instructions are available
      -- and selected.

    = DataNodeType {
          nodeData :: Data
      }

      -- | The @OpNode@ represents nodes involved in operations which consume
      -- data and/or produce data.

    | OpNodeType {
          nodeOp :: Operation
      }
      
      -- | The @TransferNode@ represents nodes involved in transfers of data
      -- located at one particular location to another. If the data locations
      -- belong to different storage classes, then an operation is necessary to
      -- enable that transfer. In contrast, if the data locations belong to the
      -- same storage class, then no such operation is required. In traditional
      -- compilers this transfer has been treated either through approximation
      -- or as a post-step in code emission (i.e. after instruction selection
      -- and register allocation has been performed), but through the use of
      -- data nodes this transfer becomes apparent already in the instruction
      -- selection phase.

    | TransferNodeType {
      }

      -- | The @BranchNodeType@ represents nodes involved in branching from one
      -- basic code block to another.

    | BranchNodeType {
      }

      -- | The @LabelNodeType@ represents nodes which denote code block labels.
      
    | LabelNodeType {
      }
      
      -- | The @StateNodeType@ represents nodes involved in enforcing execution
      -- order between two or more operations.

    | StateNodeType {
      }

    deriving (Show)

-- | Record for describing a node in the control and data flow graph. Each node
-- is described by a @NodeType@, and each node can also have an arbitrary set of
-- constraints applied to it (although this list may of course be empty).

data Node
    = Node {
          nodeType :: NodeType
        , nodeConstraints :: [Constraint]
      }
    deriving (Show)

-- | Record for embedding edge information. Currently this information consists
-- of numbers which allow the edges to be ordered with respect to input to and
-- output from a node. Hence, the numbering of the outputs are only related to
-- the source of the edge, and the numbering of the inputs are only related to
-- the destination of the edge. The edges must be numbered such that each input
-- to or output from a node has a strictly increasing number, starting from
-- 0. There must be no gaps in the numbers.

data EdgeInfo
    = EdgeInfo {

          -- | Edge number with regard to the outputs from a node.

          outputEdgeNumber :: Natural

          -- | Edge number with regard to the inputs to a node.

        , inputEdgeNumber :: Natural

      }
    deriving (Show)

-- | Record for describing a directed, unified control and data flow graph
-- (CDFG). The graph is represented internally using
-- @Data.Graph.Inductive.Tree.Gr@ where the nodes consists of @Node@s and the
-- edges of @EdgeInfo@s.

data CDFG 
    = CDFG {
          cdfgGraph :: Gr Node EdgeInfo
      }
    deriving (Show)

-- | Record for describing a function. A @Function@ is, like @Module@, a unit
-- which can be compiled on its own. Each @Function@ consists of a unified
-- control and data flow graph (CDFG) which describes the possible flows of
-- execution and data dependencies for the @Function@.

data Function
    = Function {
          cdfg :: CDFG
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