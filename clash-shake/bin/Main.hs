{-# LANGUAGE TemplateHaskellQuotes #-}

module Main where

import Clash.Shake

import           Clash.Shake.Example (topEntity)
import qualified Cabal.Plan as Plan
import qualified Data.Text as Text

exampleProject :: ClashProject
exampleProject = ClashProject{
    cpTarget=Precompiled 'topEntity
  , cpDependencies=Just (CabalPlan (Plan.ProjectRelativeToDir "."))
  }

main :: IO ()
main = do
  let plan = CabalPlan (Plan.ProjectRelativeToDir ".")
  env <- resolveDependencies plan
--  Map.keys $ Plan.pjUnits $ env


  putStrLn $ Text.unpack (buildGhcEnvironmentFile env)
