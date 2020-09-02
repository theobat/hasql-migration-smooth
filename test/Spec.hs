{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}

import Protolude
import Test.Tasty ( defaultMain, testGroup, TestTree )
import Dependency (getDependencyFromMigrationArgs)
import MigrationTypes (debugMode, 
  MigrationOrderedRegister(MigrationOrderedRegister, initRevList, seedList), MigrationArgs(MigrationArgs),
  MigrationRegister(dependencyMap)
  )
import MassaliaMigration (findAndOrderAllMigration, defaultMigrationPattern)
import BuildMigrationRegister (gatherFileFailOnError)
import Test.Tasty.HUnit ( testCase, assertEqual )
import qualified Data.Set as Set


main :: IO ()
main = defaultMain tests

testConfig = defaultMigrationPattern{debugMode=False}

tests :: TestTree
tests = testGroup
    "parse dependencies"
    [ testCase "parse single file ddli_a.sql" $ do
        arg <- getDependencyFromMigrationArgs (MigrationArgs "ddli_a.sql" "./test/ddli_a.sql")
        assertEqual "" (Set.fromList ["ddli_b.sql","ddli_c.sql"]) arg,
      testCase "parse single file ddli_b.sql" $ do
        arg <- getDependencyFromMigrationArgs (MigrationArgs "ddli_b.sql" "./test/ddli_b.sql")
        assertEqual "" (Set.fromList ["ddli_c.sql"]) arg,
      testCase "parse single file without dependencies ddli_c.sql" $ do
        arg <- getDependencyFromMigrationArgs (MigrationArgs "ddli_c.sql" "./test/ddli_c.sql")
        assertEqual "" (Set.fromList []) arg,
      testCase "build migration register" $ do
        arg <- runExceptT $ dependencyMap <$> gatherFileFailOnError defaultMigrationPattern
        let expected = "Right (fromList [(\"ddli_c.sql\",fromList []),(\"ddli_a.sql\",fromList [\"ddli_b.sql\",\"ddli_c.sql\"]),(\"ddli_0.sql\",fromList [\"ddli_a.sql\",\"ddli_b.sql\",\"ddli_c.sql\"]),(\"ddli_b.sql\",fromList [\"ddli_c.sql\"])])"
        assertEqual @Text "" expected (show arg),
      testCase "build migration ordered register" $ do
        arg <- runExceptT $ findAndOrderAllMigration defaultMigrationPattern
        let expected = "Right (MigrationOrderedRegister {initRevList = [JustInit (MigrationArgs {scriptName = \"ddli_c.sql\", path = \"./test/ddli_c.sql\"}),JustInit (MigrationArgs {scriptName = \"ddli_b.sql\", path = \"./test/ddli_b.sql\"}),InitAndRev (MigrationArgs {scriptName = \"ddli_a.sql\", path = \"./test/ddli_a.sql\"}) (MigrationArgs {scriptName = \"ddlr_a.sql\", path = \"./test/ddlr_a.sql\"}),JustInit (MigrationArgs {scriptName = \"ddli_0.sql\", path = \"./test/ddli_0.sql\"})], seedList = []})"
        assertEqual @Text "" expected (show arg)
    ]
