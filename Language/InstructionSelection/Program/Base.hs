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
-- Contains the data and types for representing programs as unified control and
-- data flow graphs on a per-function basis.
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

      -- | Represents all data values which are fixed and known at compile time.

    = Constant {
          constValue :: Integer
      }

      -- | Represents all data values which are fixed but unknown from the scope
      -- of where it is used. It is intended to be used within patterns only and
      -- should never appear as part of the program. The idea is to represent
      -- immediate values in patterns through symbolic names which will be
      -- replaced by the constant values upon code emission.

    | Immediate {
          immId :: String
      }

      -- | Represents all temporary data values.

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
-- Concerning floating-point: A floating-point value can either denote a \real
-- value\ or \not a number\ (NaN) which indicate errors. NaNs in turn can be
-- classified as \quiet NaNs\ (QNaN) and \signaling NaNs\ (SNaN).
--
-- For comparison, see 'SideEffectOperation'.

data PureOperation

    ------------------------------------------------------------
    -- Arithmetic and logical operations
    ------------------------------------------------------------

    ------------------------------
    -- Integer operations
    ------------------------------

    -- | Integer addition. Consumes 2 data nodes and produces 1 data
    -- node. Commutative.

    = IAdd

    -- | Integer subtraction. Consumes 2 data nodes and produces 1 data node. If
    -- data at input 0 is denoted by @x@, and data at input 1 is denoted by @y@,
    -- then this operation represents @x - y@.

    | ISub

    -- | Integer multiplication. Consumes 2 data nodes and produces 1 data
    -- node. Commutative.

    | IMul

    -- | Unsigned integer division. Consumes 2 data nodes and produces 1 data
    -- node. If data at input 0 is denoted by @x@, and data at input 1 is
    -- denoted by @y@, then this operation represents @x / y@.

    | IUDiv

    -- | Unsigned integer remainder. Same as for 'IUDiv' but for signed integer
    -- data.

    | ISDiv

    -- | Unsigned integer remainder. Consumes 2 data nodes and produces 1 data
    -- node. If data at input 0 is denoted by @x@, and data at input 1 is
    -- denoted by @y@, then this operation represents @x % y@.

    | IURem
      
    -- | Signed integer remainder. Same as for 'IURem' but for signed integer
    -- data.

    | ISRem

    ------------------------------
    -- Floating-point operations
    ------------------------------

    -- | Float addition. Consumes 2 data nodes and produces 1 data
    -- node. Commutative.

    | FAdd

    -- | Float subtraction. Consumes 2 data nodes and produces 1 data node. If
    -- data at input 0 is denoted by @x@, and data at input 1 is denoted by @y@,
    -- then this operation represents @x - y@.

    | FSub

    -- | Float multiplication. Consumes 2 data nodes and produces 1 data
    -- node. Commutative.

    | FMul

    -- | Float division. Consumes 2 data nodes and produces 1 data node. If data
    -- at input 0 is denoted by @x@, and data at input 1 is denoted by @y@, then
    -- this operation represents @x / y@.

    | FDiv

    -- | Float remainder. Consumes 2 data nodes and produces 1 data node. If
    -- data at input 0 is denoted by @x@, and data at input 1 is denoted by @y@,
    -- then this operation represents @x % y@.

    | FRem
      
    ------------------------------
    -- Bit operations
    ------------------------------

    -- | Bitwise left shift. Consumes 2 data node and produces 1 data node. If
    -- data at input 0 is denoted by @x@, and data at input 1 is denoted by @y@,
    -- then this operation represents @x < y@.

    | Shl

    -- | Bitwise logical right shift. Consumes 2 data node and produces 1 data
    -- node. If data at input 0 is denoted by @x@, and data at input 1 is
    -- denoted by @y@, then this operation represents @x > y@.

    | LShr
      
    -- | Bitwise arithmetic right shift (with sign extension). Consumes 2 data
    -- node and produces 1 data node. If data at input 0 is denoted by @x@, and
    -- data at input 1 is denoted by @y@, then this operation represents @x >
    -- y@.

    | AShr

    -- | Bitwise AND (@\&@). Consumes 2 data node and produces 1 data
    -- node. Commutative.

    | And
      
    -- | Bitwise OR (@|@). Consumes 2 data node and produces 1 data
    -- node. Commutative.

    | Or

    -- | Bitwise XOR (@^@). Consumes 2 data node and produces 1 data
    -- node. Commutative.

    | Xor

    ------------------------------------------------------------
    -- Comparison operations
    ------------------------------------------------------------

    ------------------------------
    -- Integer operations
    ------------------------------

    -- | Integer equality comparison (@==@). Consumes 2 data node and produces 1
    -- data node. Commutative.

    | ICmpEq
      
    -- | Integer inequality comparison (@!=@). Consumes 2 data node and produces
    -- 1 data node. Commutative.

    | ICmpNEq
      
    -- | Unsigned integer greater-than comparison (@>@). Consumes 2 data node
    -- and produces 1 data node. If data at input 0 is denoted by @x@, and data
    -- at input 1 is denoted by @y@, then this operation represents @x > y@.

    | IUCmpGT
      
    -- | Signed integer greater-than comparison (@>@). Same as for 'IUCmpGT' but
    -- for signed integer data.

    | ISCmpGT
      
    -- | Unsigned integer greater-than-or-equal comparison (@>=@). Consumes 2
    -- data node and produces 1 data node. If data at input 0 is denoted by @x@,
    -- and data at input 1 is denoted by @y@, then this operation represents @x
    -- >= y@.

    | IUCmpGE
      
    -- | Signed integer greater-than-or-equal comparison (@>=@). Same as for
    -- 'IUCmpGE' but for signed integer data.

    | ISCmpGE
      
    -- | Unsigned integer less-than comparison (@<@). Consumes 2 data node and
    -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
    -- input 1 is denoted by @y@, then this operation represents @x < y@.

    | IUCmpLT
      
    -- | Signed integer less-than comparison (@<@). Same as for 'IUCmpLT' but
    -- for signed integer data.

    | ISCmpLT
      
    -- | Unsigned integer less-than-or-equal comparison (@<=@). Consumes 2 data
    -- node and produces 1 data node. If data at input 0 is denoted by @x@, and
    -- data at input 1 is denoted by @y@, then this operation represents @x <=
    -- y@.

    | IUCmpLE
      
    -- | Signed integer less-than-or-equal comparison (@<=@). Same as for
    -- 'IUCmpLE' but for signed integer data.

    | ISCmpLE
      
    ------------------------------
    -- Floating-point operations
    ------------------------------

    -- | Unordered float equality comparison (@==@). Consumes 2 data node and
    -- produces 1 data node. If any of the input values is a QNaN or both values
    -- are equal, then the operation returns @True@. Commutative.

    | FUCmpEq
      
    -- | Ordered float inequality comparison (@!=@). Consumes 2 data node and
    -- produces 1 data node. If none of the input values is a QNaN and both
    -- values are equal, then the operation returns @True@. Commutative.
      
    | FOCmpEq
      
    -- | Unordered float inequality comparison (@!=@). Consumes 2 data node and
    -- produces 1 data node. If any of the input values is a QNaN or both values
    -- are inequal, then the operation returns @True@. Commutative.

    | FUCmpNEq
      
    -- | Ordered float inequality comparison (@!=@). Consumes 2 data node and
    -- produces 1 data node. If none of the input values is a QNaN and both
    -- values are inequal, then the operation returns @True@. Commutative.
      
    | FOCmpNEq
      
    -- | Unordered float greater-than comparison (@>@). Consumes 2 data node and
    -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
    -- input 1 is denoted by @y@, then this operation represents @x > y@. Hence,
    -- if any of the input values is a QNaN or @x > y@ holds, then the operation
    -- returns @True@.

    | FUCmpGT
      
    -- | Ordered float greater-than comparison (@>@). Consumes 2 data node and
    -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
    -- input 1 is denoted by @y@, then this operation represents @x > y@. Hence,
    -- if none of the input values is a QNaN and @x > y@ holds, then the
    -- operation returns @True@.

    | FOCmpGT
      
    -- | Unordered float greater-than-or-equal comparison (@>=@). Consumes 2
    -- data node and produces 1 data node. If data at input 0 is denoted by @x@,
    -- and data at input 1 is denoted by @y@, then this operation represents @x
    -- >= y@. Hence, if any of the input values is a QNaN or @x >= y@ holds,
    -- then the operation returns @True@.

    | FUCmpGE
      
    -- | Ordered float greater-than-or-equal comparison (@>=@). Consumes 2 data
    -- node and produces 1 data node. If data at input 0 is denoted by @x@, and
    -- data at input 1 is denoted by @y@, then this operation represents @x >=
    -- y@. Hence, if none of the input values is a QNaN and @x >= y@ holds, then
    -- the operation returns @True@.

    | FOCmpGE
      
    -- | Unordered float less-than comparison (@<@). Consumes 2 data node and
    -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
    -- input 1 is denoted by @y@, then this operation represents @x < y@. Hence,
    -- if any of the input values is a QNaN or @x < y@ holds, then the operation
    -- returns @True@.

    | FUCmpLT
      
    -- | Ordered float less-than comparison (@<@). Consumes 2 data node and
    -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
    -- input 1 is denoted by @y@, then this operation represents @x < y@. Hence,
    -- if none of the input values is a QNaN and @x < y@ holds, then the
    -- operation returns @True@.

    | FOCmpLT
      
    -- | Unordered float less-than-or-equal comparison (@<=@). Consumes 2 data
    -- node and produces 1 data node. If data at input 0 is denoted by @x@, and
    -- data at input 1 is denoted by @y@, then this operation represents @x <=
    -- y@. Hence, if any of the input values is a QNaN or @x <= y@ holds, then
    -- the operation returns @True@.

    | FUCmpLE
      
    -- | Ordered float less-than-or-equal comparison (@<=@). Consumes 2 data
    -- node and produces 1 data node. If data at input 0 is denoted by @x@, and
    -- data at input 1 is denoted by @y@, then this operation represents @x <=
    -- y@. Hence, if none of the input values is a QNaN and @x <= y@ holds, then
    -- the operation returns @True@.

    | FOCmpLE

    -- | Float unordering check. Consumes 2 data nodes and produces 1 data node.
    -- If any of the input values is a QNaN, then the operation returns
    -- @True@. Commutative.

    | FCmpUn 

    ------------------------------------------------------------
    -- Other operations
    ------------------------------------------------------------

    -- | 2-value-input phi operation. Consumes 2 tuples of data nodes and label
    -- nodes and 1 label node and produces 1 data node. Input 0 is expected to
    -- always be the label wherein the phi operation resides, and inputs 1 and 2
    -- are the tuples. Inputs 1 and 2 are commutative.

    | Phi

    -- | 1-value-input phi operation. Consumes 1 tuple of data node and label
    -- node and 1 data node and produces 1 data node. Input 0 is expected to
    -- always be a data node produced by another 'Phi' or 'PhiCascade' node.

    | PhiCascade
    
    deriving (Show)
             
-- | Record for describing a side-effect data operation that consumes and/or
-- produces data. Side-effect operations are those that depend on or modify some
-- external state, meaning that they can produce different results even if the
-- set of inputs is the same. Consequently the execution order in which
-- side-effect operations are arranged with respect to other side-effect
-- operations must be maintained. Hence, side-effect operations are the opposite
-- of pure operations.
--
-- To enforce this execution order, all side-effect operations consume exactly
-- one state node and produce exactly one state node (of course, they may
-- consume and produce data nodes in addition to the state nodes). If a
-- side-effect operation A consumes the state produce by another side-effect
-- operation B, then B must be scheduled before A.
--
-- The edge from the state node to the side-effect operation node must always
-- have input number 0 (i.e. it is always the first edge into the side-effect
-- operation node).
--
-- For comparison, see 'PureOperation'.

data SideEffectOperation

    ------------------------------------------------------------
    -- Memory operations
    ------------------------------------------------------------

    -- | Memory load.

    = MemLoad
      
    -- | Memory store.

    | MemStore

    ------------------------------------------------------------
    -- Function calls
    ------------------------------------------------------------

      -- | Immediate call, where the function to invoke is determined via a
      -- function label.
      
    | ImmediateCall
      
      -- | Indirect call, where the function to invoke is determined via a
      -- register.

    | IndirectCall
    
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
             
-- | Record for a branching.

data Branching

      -- | Unconditional jump.

    = Jmp

      -- | Conditional jump.

    | CondJmp

    deriving (Show)



------------------------------------------------------------
-- Control and data flow elements
------------------------------------------------------------

-- | Record for describing the node type.

data NodeType

      -- | The 'DataNode' represents nodes which denote data. The data may need,
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

      -- | The 'OpNode' represents nodes involved in operations which consume
      -- data and/or produce data.

    | OpNodeType {
          nodeOp :: Operation
      }
      
      -- | The 'TransferNode' represents nodes involved in transfers of data
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
          -- TODO: add types? Maybe it's not needed
      }

      -- | The 'BranchNodeType' represents nodes involved in branching from one
      -- basic code block to another.

    | BranchNodeType {
          branch :: Branching
      }

      -- | The 'LabelNodeType' represents nodes which denote code block labels.
      
    | LabelNodeType {
          -- TODO: add types
      }
      
      -- | The 'StateNodeType' represents nodes involved in enforcing execution
      -- order between two or more operations.

    | StateNodeType {
          -- TODO: add
      }

    deriving (Show)

-- | Record for describing a node in the control and data flow graph. Each node
-- is described by a 'NodeType', and each node can also have an arbitrary set of
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
-- 'Data.Graph.Inductive.Tree.Gr' where the nodes consists of 'Node's and the
-- edges of 'EdgeInfo's.

data CDFG 
    = CDFG {
          cdfgGraph :: Gr Node EdgeInfo
      }
    deriving (Show)

-- | Record for describing a function. A 'Function' is, like 'Module', a unit
-- which can be compiled on its own. Each 'Function' consists of a unified
-- control and data flow graph (CDFG) which describes the possible flows of
-- execution and data dependencies for the 'Function'.

data Function
    = Function {
          cdfg :: CDFG
      }
    deriving (Show)

-- | Record for describing a module. A 'Module' is a self-contained unit which
-- can be compiled on its own. In turn, each 'Module' consists of a set of
-- 'Function's, which also are compiled in isolation.

data Module
    = Module {

          -- | List of 'Function's which comprises the 'Module'. The 'Module'
          -- must consist of at least one 'Function'.

          functions :: [Function]

      }
    deriving (Show)

-- | Record for describing a program. All 'Program's consists of a set of
-- 'Module's, which is a self-contained unit which can be compiled on its own
-- (dependencies on other functions are resolved at linking time, not compile
-- time). This is the outer-most level of data records.

data Program 
    = Program {

          -- | List of 'Module's which comprises the 'Program'. The 'Program'
          -- must consist of at least one 'Module'.

          modules :: [Module]

      }
    deriving (Show)