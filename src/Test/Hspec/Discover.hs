-- | This functions are used by @hspec-discover@.  They are not part of the
-- public API and may change at any time.
module Test.Hspec.Discover (Spec, hspec, hspecWithFormatter, postProcessSpec, describe) where

import           Test.Hspec
import           Test.Hspec.Runner
import           Test.Hspec.Formatters

import           Control.Applicative
import           Data.List

import           Test.Hspec.Core.Type hiding (describe)
import           Test.Hspec.Util

hspecWithFormatter :: IsFormatter a => a -> Spec -> IO ()
hspecWithFormatter formatter spec = do
  f <- toFormatter formatter
  hspecWith defaultConfig {configFormatter = Just f} spec

postProcessSpec :: FilePath -> Spec -> Spec
postProcessSpec = locationHeuristicFromFile

locationHeuristicFromFile :: FilePath -> Spec -> Spec
locationHeuristicFromFile file spec = do
  input <- either (const Nothing) Just <$> (runIO . safeTry . readFile) file
  addLoctions (lookupLocation file input) spec

addLoctions :: (String -> Maybe Location) -> Spec -> Spec
addLoctions lookupLoc = mapSpecTree go
  where
    go :: SpecTree -> SpecTree
    go spec = case spec of
      SpecGroup d es -> SpecGroup d (map go es)
      BuildSpecs es -> BuildSpecs (map go <$> es)
      SpecWithCleanup cleanup e -> SpecWithCleanup cleanup (go e)
      SpecItem requirement item -> SpecItem requirement item {itemLocation = itemLocation item <|> lookupLoc requirement}

lookupLocation :: FilePath -> Maybe String -> String -> Maybe Location
lookupLocation file input requirement = loc
  where
    loc :: Maybe Location
    loc = Location file <$> line <*> pure 0 <*> pure BestEffort

    line :: Maybe Int
    line = fst <$> (inputLines >>= find p)
      where
        p :: (Int, String) -> Bool
        p = isInfixOf (show requirement) . snd

    inputLines :: Maybe [(Int, String)]
    inputLines = zip [1..] . lines <$> input
