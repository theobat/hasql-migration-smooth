module Utils where

import Data.UUID.V4 (nextRandom)
import Data.UUID (UUID)
import Protolude
import MigrationTypes
    ( TupleMigration(..),
      MigrationRegister(initRevMap, seed),
      MigrationPattern(migrationPatternList, basePath,
                       seedMigrationPrefix, revisionMigrationPrefix, initMigrationPrefix),
      MigrationArgs(MigrationArgs, scriptName, path),
      AllMigration(..),
      InitOrRevMigration(..) )
import Hasql.Migration ( ScriptName )
import Data.Map (Map)
import qualified Data.Map as Map ( insert, updateLookupWithKey )
import System.FilePath.Posix (splitFileName, (</>))
import System.FilePattern.Directory (FilePattern, getDirectoryFiles)

uuidV4 :: IO UUID
uuidV4 = nextRandom

dropPrefixFromName :: String -> ScriptName -> ScriptName
dropPrefixFromName prefix = drop (length prefix)

getInitMigration :: TupleMigration -> MigrationArgs
getInitMigration input = case input of
  JustInit init -> init
  InitAndRev init _ -> init

getMaybeInit :: InitOrRevMigration -> Maybe MigrationArgs
getMaybeInit input = case input of
  Init _ migrationArg -> Just migrationArg
  _ -> Nothing
  
-- | Smaller means it's processed before, greater means after.
registerBuildingProcessingOrder :: AllMigration -> Int
registerBuildingProcessingOrder input = case input of
  (Seed _) -> 1
  (InitOrRev (Rev _ _)) -> 2
  (InitOrRev (Init _ _)) -> 3