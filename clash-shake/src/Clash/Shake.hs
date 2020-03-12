{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Shake where

import qualified Path as Path
import           Path (Path)
import           Path ((</>))
import           Control.Monad.Catch (MonadThrow)
import qualified Cabal.Plan as Plan
import           Clash.Util (concatMapM)
import qualified Data.Text as Text
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Data.Set (Set)
import qualified Language.Haskell.TH as TH
import           TextShow (showt)

--import qualified Clash.Util.Interpolate as I
--import qualified Language.Haskell.TH.Syntax as TH
import Debug.Trace

-- | See GHC User's Guide for more information:
-- https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/packages.html
data Dependency
  = PackageEnv FilePath
  -- ^ Corresponds to '-package-env envFile' on commandline
  | PackageDb (Path Path.Abs Path.Dir)
  -- ^ Corresponds to '-package-db pkgDb' on commandline
  | Package Plan.UnitId
  -- ^ Corresponds to '-package pkg' on commandline
  deriving (Show, Eq, Ord)

data DependencyLocation
  = CabalPlan Plan.SearchPlanJson
  -- ^ Load dependencies from a plan.json file, usually located in
  -- "dist-newstyle/cache/plan.json" right after using Cabal 3.0's build.
  deriving (Show)

data CompileTarget
  = Precompiled TH.Name
  deriving (Show)

data ClashProject = ClashProject
  { cpTarget :: CompileTarget
  -- ^ Name of top entity to compile. Example: 'myTopEntity'.
  , cpDependencies :: Maybe DependencyLocation
  -- ^ How to load dependencies. If left Nothing, Clash will only load entities
  -- from the global package db.
  } deriving (Show)

emptyClashProject :: CompileTarget -> ClashProject
emptyClashProject ct = ClashProject {cpTarget=ct, cpDependencies=Nothing}

dependencyToFlag :: Dependency -> Text.Text
dependencyToFlag (PackageEnv t) = "package-env " <> Text.pack t
dependencyToFlag (PackageDb db) = "package-db " <> Text.pack (Path.fromAbsDir db)
dependencyToFlag (Package (Plan.UnitId t)) = "package-id " <> t

buildGhcEnvironmentFile :: Set Dependency -> Text.Text
buildGhcEnvironmentFile deps =
  let flags = map dependencyToFlag (Set.toList deps) in
  Text.intercalate "\n" ("clear-package-db" : "global-package-db" : flags)

-- Format PkgId as "ghc-8.2.2"
formatPkgId :: Plan.PkgId -> Text.Text
formatPkgId (Plan.PkgId (Plan.PkgName pkgName) (Plan.Ver version)) =
  pkgName <> "-" <> Text.intercalate "." (map showt version)

-- |
resolveDependencies :: DependencyLocation -> IO (Set Dependency)
resolveDependencies (CabalPlan searchPlan) = do
  planPath0 <- Plan.findPlanJson searchPlan
  plan <- Plan.decodePlanJson planPath0

  -- Figure out where packagedb in dist-newstyle is
  distNewstyleDir <- Path.parent . Path.parent <$> Path.parseAbsFile planPath0
  ghcVersion <- Path.parseRelDir (Text.unpack (formatPkgId (Plan.pjCompilerId plan)))
  let newstyleDbDir = distNewstyleDir </> $(Path.mkRelDir "packagedb") </> ghcVersion

  -- Extract local store (bit hacky.. the proper way would be to read
  -- improved-plan, but we can't because it's a pickled file)
  localDbDirs <- concatMapM localDb (Map.elems (Plan.pjUnits plan))
  let dbs = PackageDb newstyleDbDir : map PackageDb localDbDirs

  -- Extract all built (and pre-installed) packages from plan
  let pkgs = map (Package . Plan.uId) (Map.elems (Plan.pjUnits plan))

  return (Set.fromList (pkgs <> dbs))

 where
  -- Extract a local package database based on a Plan.Unit
  localDb :: MonadThrow m => Plan.Unit -> m [Path Path.Abs Path.Dir]
  -- Don't do anything for in-place packages:
  localDb (Plan.uDistDir -> Just _) = pure []

  -- Locate package db of external packages:
  localDb unit = catMaybes <$> mapM go (Map.elems (Plan.uComps unit))
   where
    go (Plan.ciBinFile -> Just bf) = do
      -- /home/.cabal/store/ghc-8.8.1/aeson-pretty-0.8.8-8_hash/bin/
      binDir <- Path.parent <$> Path.parseAbsFile bf

      -- /home/.cabal/store/ghc-8.8.1/aeson-pretty-0.8.8-8_hash
      let pkgDir = Path.parent binDir

      -- /home/.cabal/store/ghc-8.8.1
      let pkgsDir = Path.parent pkgDir

      -- /home/.cabal/store/ghc-8.8.1/package.db
      pure (Just (pkgsDir </> $(Path.mkRelDir "package.db")))
    go _ = pure Nothing

---- | Create a 'ClashProject' with 'cpPkg', 'cpModule', and 'cpTop' set
---- according to the name passed in.
--clashProject :: TH.Name -> ClashProject
--clashProject (TH.Name nm flavor)
--  | TH.OccName fNm <- nm
--  , TH.NameG TH.VarName (TH.PkgName pkg) (TH.ModName modNm) <- flavor
--  = emptyClashProject
--      { cpPkg=Text.pack pkg
--      , cpModule=Text.pack modNm
--      , cpTop=Text.pack fNm
--      }
--clashProject _ =
--  error [I.i|
--    Unsupported name passed to 'clashProject'. Only names refering to functions
--    in external packages are supported.
--  |]
