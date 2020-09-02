{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      : Massalia.HasqlConnection
-- Description : A module to enhance "Hasql.Migration" baseline.
--  The idea is to split Data Definition Language migrations in two kinds:
--    - The <<Init>> which are supposed to be executed only once.
--    - The <<Revision>> which are supposed to be executed only once.
--  If an init migration has not been executed, it's executed, if it has been and
--  the file has not changed, nothing happens. But if the file has changed and there's a
--  revision migration for this init migration we execute the revision and replace the
--  file MD5 with the new one.
module MassaliaMigration where

-- CheckScriptResult(ScriptOk, ScriptModified, ScriptNotExecuted)

import qualified Data.Map as Map ( insert, updateLookupWithKey )
import Data.String ( IsString(fromString), String )
import Hasql.Connection as Connection ( Connection, release )
import Hasql.Migration
  ( MigrationCommand (MigrationInitialization, MigrationScript),
    MigrationError (ScriptChanged),
    ScriptName,
    loadMigrationFromFile,
    runMigration,
    updateChecksum,
  )
import Hasql.Session (QueryError, run)
import qualified Hasql.Transaction as Tx ( Transaction, sql )
import qualified Hasql.Transaction.Sessions as Txs
    ( transaction, IsolationLevel(ReadCommitted), Mode(Write) )
import Hasql.URL (InitDBConnectionError, connectionFromURL)
import Protolude
import System.FilePath.Posix (splitFileName, (</>))
import System.FilePattern.Directory (FilePattern, getDirectoryFiles)
import Utils
    ( uuidV4 )
import BuildMigrationRegister
    ( FileGatheringError, gatherFileFailOnError ) 
import MigrationTypes
    ( TupleMigration(..),
      MigrationOrderedRegister(initRevList, seedList),
      DBSchemaOption(..),
      MigrationOrder(SQLFirstLine),
      MigrationPattern(..),
      MigrationArgs(..) )
import Dependency (orderMigrationRegister)

data GlobalMigrationError
  = StepInitDBError InitDBConnectionError
  | StepFileGatheringError FileGatheringError
  | StepFileExecutionError MigrationExecutionError
  deriving (Eq, Show)

data MigrationExecutionError
  = HasqlQueryError QueryError
  | HasqlMigrationError MigrationError
  | InitFileChangeWihtoutRev FilePath
  | InitChecksumUpdateError QueryError
  deriving (Eq, Show)

-- | A Massalia migration consists of two types of migration, the **init** migrations and
-- the **revision** migrations. The init migrations are the one applied and tracked using
-- "Hasql.Migration". A revision migration alone is never used.
-- The idea is to find all the init migrations and try to apply them using 'loadMigrationFromFile'
-- (based on the initMigrationPrefixValue)

-- init => applied first:
--          - if not applied, executed following Hasql.Migration concept
--          - if not applied, executed following Hasql.Migration concept
--
-- revision =>
-- Revision ==


defaultDBSchemaOption :: DBSchemaOption
defaultDBSchemaOption =
  DBSchemaOption
    { withinSchema = "public",
      setSearchPathTo = ["public"],
      commandBeforeEverything = Nothing
    }

defaultMigrationPattern :: MigrationPattern
defaultMigrationPattern =
  MigrationPattern
    { initMigrationPrefix = "ddli",
      revisionMigrationPrefix = "ddlr",
      seedMigrationPrefix = Just "dml",
      migrationPatternList = ["**/*.sql"],
      basePath = "./",
      dependencyPrefixToAdd = "ddli_",
      dbSchemaOption = Nothing,
      migrationOrder = SQLFirstLine,
      debugMode = True
    }

-- | A function to assemble, classify and order the migration files
-- And then execute them (and, stop in the way if any error is encountered).
findAndRunAllMigration ::
  MigrationPattern ->
  String ->
  ExceptT [GlobalMigrationError] IO ()
findAndRunAllMigration migrationPattern databaseURL = do
  orderedMigrationRegister <- withExceptT (StepFileGatheringError <$>) gatherAndOrderFile
  connection <- withStepError StepInitDBError connectionAttempt
  finalRes <- withStepError StepFileExecutionError $ executionScheme (dbSchemaOption migrationPattern) orderedMigrationRegister connection
  liftIO $ Connection.release finalRes
  where
    gatherAndOrderFile = findAndOrderAllMigration migrationPattern
    connectionAttempt = ExceptT $ connectionFromURL databaseURL
    withStepError errConstructor = withExceptT (pure . errConstructor)

-- | A function to assemble, classify and return the migration files
-- in the form of a migration register ('MigrationOrderedRegister').
findAndOrderAllMigration ::
  MigrationPattern ->
  ExceptT [FileGatheringError] IO MigrationOrderedRegister
findAndOrderAllMigration migrationPattern =
  orderMigrationRegister migrationPattern <$> gatherFileFailOnError migrationPattern

executionScheme ::
  Maybe DBSchemaOption ->
  MigrationOrderedRegister ->
  Connection ->
  ExceptT MigrationExecutionError IO Connection
executionScheme maybeSchemaOption register dbCo = do
  let prepareInitTransaction = migrationCommandToTransaction MigrationInitialization
  initAndRevListOfTransaction <- liftIO $ (sequence $ loadInitAndRevTransaction <$> (initRevList register))
  let initAndRevTransaction = sequence initAndRevListOfTransaction
  seedTransactionList <- liftIO $ sequence <$> (sequence $ seedToTransaction <$> seedList register)
  let allTransactions = initAndRevTransaction >> seedTransactionList
  let liftedTrans = sequence <$> allTransactions
  let schemaTransaction = Right <$> (fromMaybe mempty (schemaTransactions <$> maybeSchemaOption))
  let finalTransaction = schemaTransaction >> prepareInitTransaction >> liftedTrans
  restrictedErr <- liftIO $ runTx dbCo finalTransaction
  let final = join $ (first HasqlQueryError restrictedErr)
  const dbCo <$> (ExceptT $ pure final)

-- | For seed transactions every 'ScriptChanged' is catched and the script is reexecuted.
-- It would be interesting to handle this in an sql file comment.
seedToTransaction :: MigrationArgs -> IO (Tx.Transaction (Either MigrationExecutionError ()))
seedToTransaction arg = do
  cmd <- loadMigrationArgs arg
  let catchMigrationChanged input = case input of
        Left (ScriptChanged _) -> case cmd of
          MigrationScript name content -> pure ((
              Right <$> (updateChecksum name content) 
            ) >> Tx.sql content)
          _ -> panic "Partial pattern match OK here because 'rawInitMigration' is built this way"
        somethingElse -> somethingElse
  pure (pure $ Right ())

schemaTransactions :: DBSchemaOption -> Tx.Transaction ()
schemaTransactions
  DBSchemaOption
    { withinSchema = schemaName,
      setSearchPathTo = searchPathList,
      commandBeforeEverything = commandValue
    } =
    (fromMaybe (pure ()) (Tx.sql <$> commandValue))
      >> Tx.sql ("CREATE SCHEMA IF NOT EXISTS \"" <> schemaName <> "\";")
      >> Tx.sql ("SET search_path TO " <> searchPathJoined <> ";")
    where
      searchPathJoined = foldMap identity (intersperse "," searchPathList)

type InitAndRev = (MigrationArgs, MigrationArgs)

getInitMigration :: TupleMigration -> MigrationArgs
getInitMigration input = case input of
  JustInit init -> init
  InitAndRev init _ -> init

tupleMigrationToTupleTransaction ::
  (TupleMigration -> MigrationArgs) ->
  TupleMigration ->
  IO (Tx.Transaction (TupleMigration, Either MigrationExecutionError ()))
tupleMigrationToTupleTransaction acc tuple = do
  migrationCommand <- loadMigrationArgs (acc tuple)
  let transaction = migrationCommandToTransaction migrationCommand
  pure ((\trRes -> (tuple, trRes)) <$> transaction)

loadInitAndRevTransaction ::
  TupleMigration ->
  IO (Tx.Transaction (Either MigrationExecutionError ()))
loadInitAndRevTransaction tuple = case tuple of
  JustInit initMigrationArg -> do
    initMigrationCommand <- loadMigrationArgs initMigrationArg
    pure (rewritePureInitError <$> migrationCommandToTransaction (initMigrationCommand))
  InitAndRev initVal revVal -> do
    initMigrationCommand <- loadMigrationArgs initVal
    let initTransaction = migrationCommandToTransaction initMigrationCommand
    revMigrationCommand <- loadAndRenameRev revVal
    let revTransaction = migrationCommandToTransaction revMigrationCommand
    pure
      ( do
          initRes <- initTransaction
          case initRes of
            Left (HasqlMigrationError (ScriptChanged _)) -> revTransaction >> (updateChecksumIfPossible initMigrationCommand)
            _ -> pure initRes
      )
  where
    rewritePureInitError res = case res of
      Left (HasqlMigrationError (ScriptChanged filePathVal)) -> Left $ InitFileChangeWihtoutRev filePathVal
      r -> r
    updateChecksumIfPossible migrationCom = case migrationCom of
      MigrationScript name content -> Right <$> (updateChecksum name content)
      _ -> panic "Partial pattern match OK here because 'rawInitMigration' is built this way"
    loadAndRenameRev migrationArgs = do
      (MigrationScript name content) <- loadMigrationArgs migrationArgs
      uuidVal <- uuidV4
      pure $ MigrationScript (name <> "_" <> show uuidVal) content

migrationArgsToTransaction ::
  MigrationArgs ->
  IO (Tx.Transaction (Either MigrationExecutionError ()))
migrationArgsToTransaction args = migrationCommandToTransaction <$> loadMigrationArgs args

migrationCommandToTransaction ::
  MigrationCommand ->
  Tx.Transaction (Either MigrationExecutionError ())
migrationCommandToTransaction input =
  first HasqlMigrationError <$> (maybeToLeft () <$> ingestCommand runMigration input)
  where
    ingestCommand _ !(MigrationScript _ "") = pure Nothing
    ingestCommand fn !(MigrationScript name content) = fn (MigrationScript name (" -- coming from: " <> fromString name <> fromString "\n\n" <> content))
    ingestCommand fn !a = fn a

runTx :: Connection.Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con act = run (Txs.transaction Txs.ReadCommitted Txs.Write act) con

loadMigrationArgs :: MigrationArgs -> IO MigrationCommand
loadMigrationArgs MigrationArgs {scriptName = sn, path = scriptPath} = do
  putLText $ "Load content of " <> (show sn)
  loadMigrationFromFile sn scriptPath
