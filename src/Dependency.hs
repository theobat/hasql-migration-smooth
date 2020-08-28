{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Dependency where

import Data.HashMap.Strict (HashMap, mapWithKey, lookup, insert)
import Protolude
import qualified Data.Set as Set (insert, member, fromList)
import Prelude (String)
import qualified Data.ByteString.Lazy as B ( readFile, toStrict )
import Data.Text (breakOn, splitOn, strip, unpack)
import MigrationTypes
    ( TupleMigration(..),
      MigrationOrderedRegister(..),
      MigrationRegister(initRevMap, seed, dependencyMap),
      MigrationOrder(AdHoc, NoOrdering, SQLFirstLine),
      MigrationPattern(MigrationPattern, migrationOrder),
      MigrationArgs(scriptName, path) )
import Hasql.Migration (ScriptName)
import Utils ( getInitMigration )

-- | 
-- Tests : 
-- >>> import Data.HashMap.Strict (fromList)
-- >>> import qualified Data.Set as S (fromList)
-- >>> let db = fromList [(1 :: Integer, S.fromList [2, 3]), (2, S.fromList [4, 3]), (3, S.fromList [4]), (4, mempty)]
-- >>> recursivelyMergeDependencySet db
-- fromList [(1,fromList [2,3,4]),(2,fromList [3,4]),(3,fromList [4]),(4,fromList [])]
recursivelyMergeDependencySet :: (Ord a, Eq a, Hashable a) => HashMap a (Set a) -> HashMap a (Set a)
recursivelyMergeDependencySet input = mapWithKey iterator input
  where iterator key value = snd $ recursivelyMergeDependency input key (mempty, value)
-- | 
-- Tests : 
-- >>> import Data.HashMap.Strict (fromList)
-- >>> import qualified Data.Set as S (fromList)
-- >>> let db = fromList [(1 :: Integer, S.fromList [2, 3]), (2, S.fromList [4, 3]), (3, S.fromList [4]), (4, mempty)]
-- >>> recursivelyMergeDependency db 1 (S.fromList [], S.fromList [2, 3])
-- (fromList [1,2,3,4],fromList [2,3,4])
recursivelyMergeDependency :: (Ord a, Eq a, Hashable a) =>
  HashMap a (Set a) -> a -> (Set a, Set a) -> (Set a, Set a)
recursivelyMergeDependency mapping input (visitedSet, resSet) = recursiveCall
  where
    recursiveCall
      | input `Set.member` visitedSet = (upVisited, resSet <> directDependencies)
      | otherwise = foldr' tupleUpdater (upVisited, resSet <> directDependencies) directDependencies
        where tupleUpdater = recursivelyMergeDependency mapping
    directDependencies = fromMaybe mempty $ lookup input mapping
    upVisited = Set.insert input visitedSet

-- | The ordering is an execution ordering, which means 'LT' means before, and 'GT'
-- means after. If module A depends on module B we want module B to be executed __before__ module A.
--
-- Examples:
-- >>> import Data.HashMap.Strict (fromList)
-- >>> import qualified Data.Set as S (fromList)
-- >>> let db = fromList [(1 :: Integer, S.fromList [2, 3]), (2, S.fromList [4, 3]), (3, S.fromList [4]), (4, mempty), (5, S.fromList [1])]
-- >>> let finalDB = recursivelyMergeDependencySet db
-- >>> let comp = compareDependency finalDB
-- >>> [comp 1 3, comp 3 1, comp 4 3, comp 5 1]
-- [LT,GT,GT,LT]
compareDependency :: (Show a, Ord a, Hashable a) => HashMap a (Set a) -> a -> a -> Ordering
compareDependency mapping modA modB = case (modA == modB, aDepOnB, bDepOnA) of
  (True, _, _) -> EQ -- This is avoids the (log n) search cases when obviously equal
  (_, True, True) -> panic $ "Circular dependency between: " <> show modA <> " and " <> show modB
  (_, True, _) -> LT -- A is before B (A depends on B)
  (_, _, True) -> GT -- B is before A (B depends on A)
  _ -> EQ
  where
    aDepOnB = modA `Set.member` depB
    bDepOnA = modB `Set.member` depA
    depA = fromMaybe mempty $ lookup modA mapping
    depB = fromMaybe mempty $ lookup modB mapping
nameComp :: HashMap ScriptName (Set ScriptName) -> MigrationArgs -> MigrationArgs -> Ordering
nameComp mapping a b = compareDependency mapping (scriptName a) (scriptName b)

compareInitRev :: HashMap ScriptName (Set ScriptName) -> TupleMigration -> TupleMigration -> Ordering
compareInitRev mapping a b = (getInitMigration a) `comp` (getInitMigration b)
  where comp = nameComp mapping
compareSeeds :: HashMap ScriptName (Set ScriptName) -> MigrationArgs -> MigrationArgs -> Ordering
compareSeeds mapping a b = a `comp` b
  where comp = nameComp mapping

orderMigrationRegister :: MigrationPattern -> MigrationRegister -> MigrationOrderedRegister
orderMigrationRegister MigrationPattern {migrationOrder = givenOrderFunction} register = case givenOrderFunction of
  NoOrdering -> getRes identity identity
  SQLFirstLine -> getRes (sortBy $ compareInitRev depMapping) (sortBy $ compareSeeds depMapping)
    where depMapping = dependencyMap register
  AdHoc orderFunction -> getRes (sortBy tupleMigrationOrder) (sortBy orderFunction)
    where
      tupleMigrationOrder = applyMigration getInitMigration
      applyMigration f a b = orderFunction (f a) (f b)
  where
    getRes ord1 ord2 =
      MigrationOrderedRegister
        { initRevList = ord1 defaultInitRevList,
          seedList = ord2 defaultSeedList
        }
    defaultInitRevList = toList $ initRevMap register
    defaultSeedList = seed register

-- | Reads the first line of a file and parses the dependencies if any.
-- see 'parseDependencies'.
addMigDependencies :: MigrationArgs -> MigrationRegister -> IO MigrationRegister
addMigDependencies arg currentRegister = do
  newDeps <- getDependencyFromMigrationArgs arg
  pure currentRegister{dependencyMap=insert (scriptName arg) newDeps $ dependencyMap currentRegister}

-- 
getDependencyFromMigrationArgs :: MigrationArgs -> IO (Set ScriptName)
getDependencyFromMigrationArgs arg = do
  fileContent <- B.readFile (path arg)
  pure $ parseDependencies . decodeUtf8 $ B.toStrict fileContent

-- | parse a dependency line into a list of dependencies
-- >>> parseDependencies "-- | after: a, b, c"
-- fromList ["a","b","c"]
-- >>> parseDependencies "--  "
-- fromList []
parseDependencies :: Text -> Set ScriptName
parseDependencies input = case head $ lines input of
  Nothing -> mempty
  Just firstLine -> res
    where
      res = case splitOn "after:" firstLine of
        [_, rawDepsList] -> Set.fromList (unpack . strip <$> splitOn "," rawDepsList)
        _ -> mempty

