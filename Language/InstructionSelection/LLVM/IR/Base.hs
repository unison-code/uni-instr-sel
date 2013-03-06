--------------------------------------------------------------------------------
-- |
-- Module      :  Language.LLVM.IR.Base
-- Copyright   :  (c) Gabriel Hjort Blindell 2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  ghb@kth.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Contains the data and types most commonly used for representing LLVM IR code.
--
--------------------------------------------------------------------------------

module Language.LLVM.IR.Base where

------------------------------------------------------------
-- LLVM IR data location elements
------------------------------------------------------------

-- | Record for an LLVM IR temporary.

data Temporary
    = Temporary {

          -- | Identifier for the temporary. All temporaries are required to
          -- have a unique identifier within the function scope.

          tempId :: Integer

      }
    deriving (Show, Eq)

-- | Record for an LLVM IR immediate value.

data Immediate

      -- | Constructor for a named immediate value (i.e. an immediate which is
      -- identified by a symbol but not yet necessarily assigned a value).

    = ImmediateSymbol {

          -- | Symbol identifier.

          immId :: String

      }

      -- | Constructor for an assigned immediate value.

    | ImmediateValue {

          -- | Immediate value.

          immValue :: Integer

      }

    deriving (Show, Eq)

------------------------------------------------------------
-- LLVM IR statement and code elements
------------------------------------------------------------

-- | Record containing an LLVM IR statement.

data Statement
    = Statement {

          -- | An LLVM IR statement.

          statement :: String
      }
    deriving (Show)

-- | Record containing code that consists of a list of LLVM IR statements.

data Code
    = Code {

          -- | Chunk of LLVM IR code. The order of the statements is
          -- significant: list ordering of left to right corresponds to code
          -- ordering of top to bottom.

          cCode :: [Statement]

      }
    deriving (Show)

-- | Record containing LLVM IR code that belongs to a particular block.

data Block
    = Block {

          -- | Code isolated to a single block.

          bCode :: Code

      }
    deriving (Show)

-- | Record for reprensenting a function in LLVM.

data Function
    = Function {

          -- | Function body. The order of blocks is significant and must be as
          -- provided by LLVM.

          blocks :: [Block]

      }
    deriving (Show)
