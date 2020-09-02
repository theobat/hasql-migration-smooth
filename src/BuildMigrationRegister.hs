module BuildMigrationRegister where

import Utils ( dropPrefixFromName, getInitMigration, getMaybeInit, registerBuildingProcessingOrder) 
import Protolude
import MigrationTypes
    (debugMode,  TupleMigration(..),
      MigrationRegister(initRevMap, seed, dependencyMap),
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
import Dependency ( addMigDependencies, recursivelyMergeDependencySet )
import Text.Pretty.Simple (pPrint)

data FileGatheringError
  = FileHasNoMigrationType FilePath
  | RevMigrationWithoutInitMigration FilePath
  deriving (Eq, Show)

gatherFileFailOnError :: MigrationPattern -> ExceptT [FileGatheringError] IO MigrationRegister
gatherFileFailOnError migPattern = ExceptT $ tupleToEither <$> gatherAllMigrationFiles migPattern
  where
    tupleToEither inputTuple = case inputTuple of
      ([], result) -> Right result
      (errList, _) -> Left errList

gatherAllMigrationFiles :: MigrationPattern -> IO ([FileGatheringError], MigrationRegister)
gatherAllMigrationFiles mig = do
  filePathList <- getDirectoryFiles (basePath mig) (migrationPatternList mig)
  (migrationErrorList, migrationRegister) <- buildMigrationRegister mig filePathList
  if (debugMode mig) then (pPrint "dependencyMap = ") else pure () 
  if (debugMode mig) then pPrint $ dependencyMap migrationRegister else pure () 
  pure (migrationErrorList, migrationRegister)

buildMigrationRegister :: MigrationPattern -> [FilePath] -> IO ([FileGatheringError], MigrationRegister)
buildMigrationRegister migrationPattern listToProcess = (fmap updateDependencyMap) <$> result
  where
    updateDependencyMap reg = reg{dependencyMap = recursivelyMergeDependencySet $ dependencyMap reg}
    result = foldr' iterator (pure ([], mempty)) orderedClassifiedList
    orderedClassifiedList = sortOn processingOrder classifiedList
    processingOrder = fromRight (-1) . fmap registerBuildingProcessingOrder
    classifiedList = classifyFile migrationPattern <$> listToProcess
    iterator :: Either FileGatheringError AllMigration -> IO ([FileGatheringError], MigrationRegister) -> IO ([FileGatheringError], MigrationRegister)
    iterator migration input = do
      (errorList, prevRegister) <- input
      case migration of
            Right allMigration -> case allMigration of
              Seed mig -> addDep (Just mig) (errorList, prevRegister {seed = pure mig <> seed prevRegister})
              InitOrRev initOrRev -> addDep (getMaybeInit initOrRev) $ setInitRev initOrRev
              where
                setInitRev input = case joinInitAndRevMigration input currentInitRevMap of
                  Left err -> (errorList <> [err], prevRegister)
                  Right newMap -> (errorList, prevRegister {initRevMap = newMap})
                currentInitRevMap = initRevMap prevRegister
                addDep Nothing input = pure input
                addDep (Just arg) (e, reg) = do
                  newReg <- addMigDependencies arg reg
                  pure (e, newReg)
            Left err -> pure (errorList <> [err], prevRegister)

-- | Takes naming (prefix) rules encoded in a 'MigrationPattern' and returns
-- a classified file among the 'AllMigration' constructor.
classifyFile :: MigrationPattern -> FilePath -> Either FileGatheringError AllMigration
classifyFile migrationPattern filePath
  | isFilenamePrefix revisionPrefix = makeInitOrRev $ Rev revisionPrefix (MigrationArgs fileName fullPath)
  | isFilenamePrefix initPrefix = makeInitOrRev $ Init initPrefix (MigrationArgs fileName fullPath)
  | isSeed = Right $ Seed (MigrationArgs fileName fullPath)
  | otherwise = Left $ FileHasNoMigrationType fullPath
  where
    makeInitOrRev = Right . InitOrRev
    fullPath = (basePath migrationPattern) </> filePath
    isSeed = maybe False isFilenamePrefix (seedMigrationPrefix migrationPattern)
    revisionPrefix = revisionMigrationPrefix migrationPattern
    initPrefix = initMigrationPrefix migrationPattern
    isFilenamePrefix prefix = prefix `isPrefixOf` fileName
    (_, fileName) = splitFileName filePath
  
joinInitAndRevMigration ::
  InitOrRevMigration ->
  Map ScriptName TupleMigration ->
  Either FileGatheringError (Map ScriptName TupleMigration)
joinInitAndRevMigration input mapp = case input of
  Init prefix migrationArgs -> Right $ insertJustInit (prepareMigration prefix migrationArgs) mapp
  Rev prefix migrationArgs -> case insertRev (prepareMigration prefix migrationArgs) mapp of
    (Nothing, _) -> Left $ RevMigrationWithoutInitMigration (path migrationArgs)
    (_, result) -> Right result
  where
    insertRev (key, value) = Map.updateLookupWithKey (adjustInit value) key
    adjustInit revMig _ (JustInit initMig) = Just $ InitAndRev initMig revMig
    adjustInit _ _ _ = Nothing
    insertJustInit (key, value) = Map.insert key (JustInit value)
    prepareMigration prefix migrationArgs =
      ( dropPrefixFromName prefix (scriptName migrationArgs),
        migrationArgs
      )
