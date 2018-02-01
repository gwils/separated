#!/usr/bin/env runhaskell
\begin{code}
{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Data.List ( nub, intercalate )
import Distribution.Package ( UnitId )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) )
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks, versionNumbers )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenPackageModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildVerbosity), fromFlag )
import Distribution.Simple.LocalBuildInfo ( withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps) )
import Distribution.Types.MungedPackageId ( MungedPackageId, mungedName, mungedVersion )
import Distribution.Types.MungedPackageName ( unMungedPackageName )
import Distribution.Types.UnqualComponentName ( unUnqualComponentName )
import Distribution.Verbosity ( Verbosity )
import System.FilePath ( (</>) )

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule (fromFlag (buildVerbosity flags)) pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule verbosity pkg lbi = do
  let dir = autogenPackageModulesDir lbi
  createDirectoryIfMissingVerbose verbosity True dir
  withLibLBI pkg lbi $ \_ libcfg -> do
    withTestLBI pkg lbi $ \suite suitecfg -> do
      rewriteFile (dir </> "Build_" ++ unUnqualComponentName (testName suite) ++ ".hs") $ unlines
        [ "module Build_" ++ unUnqualComponentName (testName suite) ++ " where"
        , "deps :: [String]"
        , "deps = " ++ (show $ formatdeps (testDeps libcfg suitecfg))
        ]
  where
    formatdeps = map (formatone . snd)
    formatone p = unMungedPackageName (mungedName p) ++ "-" ++ (intercalate "." . fmap show . versionNumbers $ mungedVersion p)

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(UnitId, MungedPackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
