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
      --
      -- Data nodes with constant values have no inputs.

    = Constant {
          constValue :: Integer
      }

      -- | Represents all data values which are fixed but unknown from the scope
      -- of where it is used. It is intended to be used within patterns only and
      -- should never appear as part of the program. The idea is to represent
      -- immediate values in patterns through symbolic names which will be
      -- replaced by the constant values upon code emission.
      --
      -- Data nodes with immediate symbols have no inputs.

    | Immediate {
          immId :: String
      }

      -- | Represents all temporary data values.
      --
      -- Data nodes with temporaries have exactly one input.

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
-- the opposite of side-effect operations and never consume or produce state
-- nodes.
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

      -- | Integer subtraction. Consumes 2 data nodes and produces 1 data
      -- node. If data at input 0 is denoted by @x@, and data at input 1 is
      -- denoted by @y@, then this operation represents @x - y@.

    | ISub

      -- | Integer multiplication. Consumes 2 data nodes and produces 1 data
      -- node. Commutative.

    | IMul

      -- | Unsigned integer division. Consumes 2 data nodes and produces 1 data
      -- node. If data at input 0 is denoted by @x@, and data at input 1 is
      -- denoted by @y@, then this operation represents @x / y@.

    | IUDiv

      -- | Unsigned integer remainder. Same as for 'IUDiv' but for signed
      -- integer data.

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
      -- data at input 0 is denoted by @x@, and data at input 1 is denoted by
      -- @y@, then this operation represents @x - y@.

    | FSub

      -- | Float multiplication. Consumes 2 data nodes and produces 1 data
      -- node. Commutative.

    | FMul

      -- | Float division. Consumes 2 data nodes and produces 1 data node. If
      -- data at input 0 is denoted by @x@, and data at input 1 is denoted by
      -- @y@, then this operation represents @x / y@.

    | FDiv

      -- | Float remainder. Consumes 2 data nodes and produces 1 data node. If
      -- data at input 0 is denoted by @x@, and data at input 1 is denoted by
      -- @y@, then this operation represents @x % y@.

    | FRem

    ------------------------------
    -- Bit operations
    ------------------------------

      -- | Bitwise left shift. Consumes 2 data node and produces 1 data node. If
      -- data at input 0 is denoted by @x@, and data at input 1 is denoted by
      -- @y@, then this operation represents @x < y@.

    | Shl

      -- | Bitwise logical right shift. Consumes 2 data node and produces 1 data
      -- node. If data at input 0 is denoted by @x@, and data at input 1 is
      -- denoted by @y@, then this operation represents @x > y@.

    | LShr

      -- | Bitwise arithmetic right shift (with sign extension). Consumes 2 data
      -- node and produces 1 data node. If data at input 0 is denoted by @x@,
      -- and data at input 1 is denoted by @y@, then this operation represents
      -- @x > y@.

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

      -- | Integer equality comparison (@==@). Consumes 2 data node and produces
      -- 1 data node. Commutative.

    | ICmpEq

      -- | Integer inequality comparison (@!=@). Consumes 2 data node and
      -- produces 1 data node. Commutative.

    | ICmpNEq

      -- | Unsigned integer greater-than comparison (@>@). Consumes 2 data node
      -- and produces 1 data node. If data at input 0 is denoted by @x@, and
      -- data at input 1 is denoted by @y@, then this operation represents @x >
      -- y@.

    | IUCmpGT

      -- | Signed integer greater-than comparison (@>@). Same as for 'IUCmpGT'
      -- but for signed integer data.

    | ISCmpGT

      -- | Unsigned integer greater-than-or-equal comparison (@>=@). Consumes 2
      -- data node and produces 1 data node. If data at input 0 is denoted by
      -- @x@, and data at input 1 is denoted by @y@, then this operation
      -- represents @x >= y@.

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

      -- | Unsigned integer less-than-or-equal comparison (@<=@). Consumes 2
      -- data node and produces 1 data node. If data at input 0 is denoted by
      -- @x@, and data at input 1 is denoted by @y@, then this operation
      -- represents @x <= y@.

    | IUCmpLE

      -- | Signed integer less-than-or-equal comparison (@<=@). Same as for
      -- 'IUCmpLE' but for signed integer data.

    | ISCmpLE

    ------------------------------
    -- Floating-point operations
    ------------------------------

      -- | Unordered float equality comparison (@==@). Consumes 2 data node and
      -- produces 1 data node. If any of the input values is a QNaN or both
      -- values are equal, then the operation returns @True@. Commutative.

    | FUCmpEq

      -- | Ordered float inequality comparison (@!=@). Consumes 2 data node and
      -- produces 1 data node. If none of the input values is a QNaN and both
      -- values are equal, then the operation returns @True@. Commutative.

    | FOCmpEq

      -- | Unordered float inequality comparison (@!=@). Consumes 2 data node
      -- and produces 1 data node. If any of the input values is a QNaN or both
      -- values are inequal, then the operation returns @True@. Commutative.

    | FUCmpNEq

      -- | Ordered float inequality comparison (@!=@). Consumes 2 data node and
      -- produces 1 data node. If none of the input values is a QNaN and both
      -- values are inequal, then the operation returns @True@. Commutative.

    | FOCmpNEq

      -- | Unordered float greater-than comparison (@>@). Consumes 2 data node
      -- and produces 1 data node. If data at input 0 is denoted by @x@, and
      -- data at input 1 is denoted by @y@, then this operation represents @x >
      -- y@. Hence, if any of the input values is a QNaN or @x > y@ holds, then
      -- the operation returns @True@.

    | FUCmpGT

      -- | Ordered float greater-than comparison (@>@). Consumes 2 data node and
      -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
      -- input 1 is denoted by @y@, then this operation represents @x >
      -- y@. Hence, if none of the input values is a QNaN and @x > y@ holds,
      -- then the operation returns @True@.

    | FOCmpGT

      -- | Unordered float greater-than-or-equal comparison (@>=@). Consumes 2
      -- data node and produces 1 data node. If data at input 0 is denoted by
      -- @x@, and data at input 1 is denoted by @y@, then this operation
      -- represents @x >= y@. Hence, if any of the input values is a QNaN or @x
      -- >= y@ holds, then the operation returns @True@.

    | FUCmpGE

      -- | Ordered float greater-than-or-equal comparison (@>=@). Consumes 2
      -- data node and produces 1 data node. If data at input 0 is denoted by
      -- @x@, and data at input 1 is denoted by @y@, then this operation
      -- represents @x >= y@. Hence, if none of the input values is a QNaN and
      -- @x >= y@ holds, then the operation returns @True@.

    | FOCmpGE

      -- | Unordered float less-than comparison (@<@). Consumes 2 data node and
      -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
      -- input 1 is denoted by @y@, then this operation represents @x <
      -- y@. Hence, if any of the input values is a QNaN or @x < y@ holds, then
      -- the operation returns @True@.

    | FUCmpLT

      -- | Ordered float less-than comparison (@<@). Consumes 2 data node and
      -- produces 1 data node. If data at input 0 is denoted by @x@, and data at
      -- input 1 is denoted by @y@, then this operation represents @x <
      -- y@. Hence, if none of the input values is a QNaN and @x < y@ holds,
      -- then the operation returns @True@.

    | FOCmpLT

      -- | Unordered float less-than-or-equal comparison (@<=@). Consumes 2 data
      -- node and produces 1 data node. If data at input 0 is denoted by @x@,
      -- and data at input 1 is denoted by @y@, then this operation represents
      -- @x <= y@. Hence, if any of the input values is a QNaN or @x <= y@
      -- holds, then the operation returns @True@.

    | FUCmpLE

      -- | Ordered float less-than-or-equal comparison (@<=@). Consumes 2 data
      -- node and produces 1 data node. If data at input 0 is denoted by @x@,
      -- and data at input 1 is denoted by @y@, then this operation represents
      -- @x <= y@. Hence, if none of the input values is a QNaN and @x <= y@
      -- holds, then the operation returns @True@.

    | FOCmpLE

      -- | Float unordering check. Consumes 2 data nodes and produces 1 data
      -- node.  If any of the input values is a QNaN, then the operation returns
      -- @True@. Commutative.

    | FCmpUn

    ------------------------------------------------------------
    -- Other operations
    ------------------------------------------------------------

      -- | Variable value-input phi operation. Consumes a variable number
      -- (although at least 2) of tuples of data nodes and label nodes and 1
      -- label node and produces 1 data node. Input 0 is expected to always be
      -- the label wherein the phi operation resides, and the remaining are the
      -- tuples (which are commutative).
      --
      -- Since the pattern matcher requires nodes with fixed number of inputs,
      -- all 'PhiVI' nodes will need to be replaced by 'PhiFI' followed by a
      -- cascaded series of 'PhiFIC' nodes. Since this is an automated process,
      -- the subject graph and pattern graphs to the instruction selection tool
      -- should only use 'PhiVI' and never 'PhiFI' and 'PhiFIC' nodes as the
      -- tool will take care of this conversion.

    | PhiVI

      -- | 2-value-input phi operation. Consumes 2 tuples of data nodes and
      -- label nodes and 1 label node and produces 1 data node. Input 0 is
      -- expected to always be the label wherein the phi operation resides, and
      -- inputs 1 and 2 are the tuples. Inputs 1 and 2 are commutative.
      --
      -- This node type should never be used in the subject or pattern graphs!
      -- Instead, only use the 'PhiVI' type.

    | PhiFI

      -- | 1-value-input phi cascade operation. Consumes 1 tuple of data node
      -- and 1 data node produced by another 'PhiFI' or 'PhiFIC' node and
      -- produces 1 data node. Input 0 is expected to always be the consumed
      -- data node.
      --
      -- This node type should never be used in the subject or pattern graphs!
      -- Instead, only use the 'PhiVI' type.
      --
      -- A 'PhiFIC' must be used in conjunction with a 'PhiFI' or another
      -- 'PhiFIC' and must thus never appear on its own inside a pattern.

    | PhiFIC

      -- | Data transfer operation. Consumes 1 data node and produces 1 data
      -- node. This is used to enable data to be transferred from one register
      -- class to another if two dataflow-related operations operate on
      -- different register classes. In traditional compilers this transfer has
      -- been treated either through approximation or as a post-step in code
      -- emission (i.e. after instruction selection and register allocation has
      -- been performed), but through the use of data nodes and transfer
      -- operations this overhead is taken into account already in the
      -- instruction selection phase.

    | Transfer

      -- | Variable parameter-input join operation. Consumes a variable number
      -- of data nodes and produces 1 data node. Commutative.
      --
      -- This is used to provide multiple parameter inputs to a function call
      -- node (see 'ImmCallFIWParams'). Since the pattern matcher requires nodes
      -- with fixed number of inputs, all 'ParamJoinVI' nodes will need to be
      -- replaced by a cascaded series of 'ParamJoinFI' nodes. Since this is an
      -- automated process, the subject graph and pattern graphs to the
      -- instruction selection tool should only use 'ParamJoinVI' and never
      -- 'ParamJoinFI' nodes as the tool will take care of this conversion.

    | ParamJoinVI

      -- | 2-parameter-input join operation. Consumes 2 data nodes and produces
      -- 1 data node. Commutative.
      --
      -- This is used to provide multiple parameter inputs to a function call
      -- node (see 'ImmCallFIWParams'). This node type should never be used in
      -- the subject or pattern graphs! Instead, only use the 'ParamJoinVI'
      -- type.

    | ParamJoinFI

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
-- The edge from the consumed state node to the side-effect operation node must
-- always have input number 0 (i.e. it is always the first edge into the
-- side-effect operation node). Similarly, the edge from the side-effect
-- operation node to the produced state node must always have output number 0
-- (i.e. it is always the first edge out from the side-effect operation node).
--
-- Concerning function calls: There is no distinction between functions that
-- return data from those that do not; all functions will be assumed to return 1
-- data node. However, for the latter, this data will then never be used
-- anywhere and thus not inflict any additional constraints on the program to be
-- generated. The reason for this is to avoid having to differentiate between
-- function with return values and those without return values, thus simplifying
-- matters.
--
-- For comparison, see 'PureOperation'.

data SideEffectOperation

    ------------------------------------------------------------
    -- Memory operations
    ------------------------------------------------------------

      -- | Memory load. Consumes 1 data node containing the address and 1 state
      -- node and produces 1 data node and 1 state node. Input 0 and output 0
      -- are expected to always be the state nodes consumed and produced,
      -- respectively.

    = MemLoad

      -- | Memory store. Consumes 2 data nodes, one containing the address and
      -- another containing the data to store, and 1 state node. Input 0 and
      -- output 0 are expected to always be the state nodes consumed and
      -- produced, respectively, and input 1 is expected to always be the
      -- address.

    | MemStore

    ------------------------------------------------------------
    -- Function calls
    ------------------------------------------------------------

      -- | Immediate call with no input parameters where the function to invoke
      -- is determined via a function label. Consumes 1 label node and 1 state
      -- node and produces 1 state node and 1 data node. Input 0 and output 0
      -- are expected to always be the states node consumed and produced,
      -- respectively.

    | ImmCallNoParams

      -- | Immediate call with one or more input parameters where the function
      -- to invoke is determined via a function label. Consumes 1 label node, 1
      -- data node and 1 state node and produces 1 state node and 1 data
      -- node. Input 0 and output 0 are expected to always be the states node
      -- consumed and produced, respectively, and input 1 is expected to always
      -- be the function label node consumed.
      --
      -- The input data node may be produced from either an operation (for a
      -- single input), a 'ParamJoinVI' operation (for multiple inputs), or
      -- through a cascaded series of 'ParamJoinFI' nodes (for multiple inputs).
      -- However, for subject and pattern graphs no 'ParamJoinFI' nodes should
      -- never be present; instead, only use the 'ParamJoinVI' node type.

    | ImmCallWParams

      -- | Indirect call. Same as 'ImmCallNoParams' but where the label node
      -- is replaced by a data node.

    | IndCallNoParams

      -- | Indirect call. Same as 'ImmCallNoParams' but where the label node
      -- is replaced by a data node.

    | IndCallWParams

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
-- Other elements
------------------------------------------------------------

-- | Record for a jump.

data Jump

      -- | Unconditional jump. Consumes no nodes and produces 1 label node.

    = Jmp

      -- | Conditional jump. Consumes 1 data node and produces 2 label nodes.
      -- Output 0 is expected to always be the label which is reached when the
      -- data is evaluated to @False@.

    | CondJmp

    deriving (Show)

-- | Record for a label of a basic code block or a function.

data Label
    = Label {
          label :: String
      }
    deriving (Show)



------------------------------------------------------------
-- Control and data flow elements
------------------------------------------------------------

-- | Record for describing the node type.

data NodeType

      -- | The 'DataNodeType' represents nodes which denote data. Such nodes are
      -- collectively called /data nodes/. The data may need, at a later point
      -- in the compilation, to be stored at a data location such as in a
      -- register or in memory. However, it may also be that the value is
      -- computed as an intermediate value as part of a complex instruction. In
      -- such instances the value is located within the pipeline and thus do not
      -- require to be allocated a particular data location. This depends of
      -- course on which instructions are available and selected.

    = DataNodeType {
          nodeData :: Data
      }

      -- | The 'OpNodeType' represents nodes involved in operations which
      -- consume data and/or produce data. Such nodes are collectively called
      -- /operation nodes/.

    | OpNodeType {
          nodeOp :: Operation
      }

      -- | The 'JumpNodeType' represents nodes involved in jumping (also known
      -- as branching) from one basic code block to another. Such nodes are
      -- collectively called /jump nodes/.

    | JumpNodeType {
          nodeJump :: Jump
      }

      -- | The 'LabelNodeType' represents nodes which denote basic code block
      -- labels or function labels. Such nodes are collectively called /label
      -- nodes/.
      --
      -- Label nodes have exactly one input. The input may come from either a
      -- jump node, a label-merger node, or a dummy node.

    | LabelNodeType {
          nodeLabel :: Label
      }

      -- | The 'StateNodeType' represents nodes involved in enforcing execution
      -- order between two or more operations. Such nodes are collectively
      -- called /state nodes/.
      --
      -- State nodes have exactly one input. If a state is an initial state
      -- (i.e. has no natural input) the input comes from a dummy node.

    | StateNodeType

      -- | The 'LabelMergerNodeType' represents nodes which consumes two
      -- identical labels and produces single identical label, and such nodes
      -- are thus called /label-merger nodes/.
      --
      -- This is needed for the pattern matching phase where each node must have
      -- a fixed number of inputs. Since a label may have an arbitrary number of
      -- branches to it, this restriction is worked around by multiplying the
      -- label nodes and then merging them through label-merger nodes. This also
      -- prevents matching of complex patterns which would yield situations
      -- where branchings would occur to an intermediate point inside the
      -- instruction (which is not possible).

    | LabelMergerNodeType

      -- | The 'DummyNodeType' represents nodes which are only used to provide
      -- input to nodes where there is no other natural source of input. Such
      -- nodes are thus called /dummy nodes/.
      --
      -- The reason behind the requirement of each node being required to have a
      -- fixed number of inputs is due to the pattern matcher algorithm. The
      -- pattern matcher operates by doing a table lookup for each node. There
      -- is a table for each node type with a dimension equal to the number of
      -- inputs to that node type. Since the tables are precompiled the number
      -- of inputs must be fixed. After matching the dummy nodes are no longer
      -- needed and can safely be removed.

    | DummyNodeType

    deriving (Show)

-- | Record for describing a node in the control and data flow graph. Each node
-- is described by a 'NodeType', and each node can also have an arbitrary set of
-- constraints applied to it (although this list may of course be empty). All
-- nodes are also assigned a node number which is required to be unique for the
-- entire graph (i.e. within each function).

data Node
    = Node {
          nodeType :: NodeType
        , nodeConstraints :: [Constraint]
        , nodeNumber :: Integer
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