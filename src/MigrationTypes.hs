{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MigrationTypes where

import Protolude
import Data.String (String)
import System.FilePattern.Directory (FilePattern)
import Hasql.Migration (ScriptName)
import Data.HashMap.Strict (HashMap)

data InitOrRevMigration = Init String MigrationArgs | Rev String MigrationArgs deriving (Eq, Show, Ord)

data AllMigration = InitOrRev InitOrRevMigration | Seed MigrationArgs deriving (Eq, Show, Ord)

type InitAndRev = (MigrationArgs, MigrationArgs)

-- splitFileName
data MigrationArgs = MigrationArgs
  { scriptName :: ScriptName,
    path :: FilePath
  }
  deriving (Eq, Show, Ord)

-- | A set of options to create/change the execution of the migration process.
data MigrationPattern = MigrationPattern
  { initMigrationPrefix :: !String,
    revisionMigrationPrefix :: !String,
    seedMigrationPrefix :: !(Maybe String),
    migrationPatternList :: ![FilePattern],
    basePath :: !FilePath,
    dbSchemaOption :: !(Maybe DBSchemaOption),
    dependencyPrefixToAdd :: String, 
    migrationOrder :: MigrationOrder
  }

data MigrationOrder
  = SQLFirstLine
  | NoOrdering
  | AdHoc (MigrationArgs -> MigrationArgs -> Ordering)

-- | Options to operate the migration(s) within a given schema.
-- This is only if you want to change the overall default schema,
-- otherwise you can specify a schema in a migration file directly.
data DBSchemaOption = DBSchemaOption
  { withinSchema :: !ByteString,
    setSearchPathTo :: ![ByteString],
    commandBeforeEverything :: !(Maybe ByteString)
  }
  deriving (Show)

data MigrationRegister = MigrationRegister
  { initRevMap :: Map ScriptName TupleMigration,
    seed :: [MigrationArgs],
    dependencyMap :: HashMap ScriptName (Set ScriptName)
  }
  deriving (Eq, Show)

data MigrationOrderedRegister = MigrationOrderedRegister
  { initRevList :: [TupleMigration],
    seedList :: [MigrationArgs]
  }
  deriving (Eq, Show)

-- | An init migration assembled
data TupleMigration
  = JustInit MigrationArgs
  | InitAndRev MigrationArgs MigrationArgs
  deriving (Eq, Show, Ord)

instance Semigroup MigrationRegister where
  (<>) a b =
    MigrationRegister
      { initRevMap = (initRevMap a <> initRevMap b),
        seed = (seed a <> seed b),
        dependencyMap = mempty
      }

instance Monoid MigrationRegister where
  mempty = MigrationRegister {initRevMap = mempty, seed = mempty, dependencyMap = mempty}


