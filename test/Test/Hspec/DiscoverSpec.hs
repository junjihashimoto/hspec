{-# LANGUAGE OverloadedStrings #-}
module Test.Hspec.DiscoverSpec (main, spec) where

import           Helper
import           Data.String
import           Data.String.Builder

import qualified Test.Hspec as H (it)
import           Test.Hspec.Core (Item(..), Location(..), LocationAccuracy(..))
import           Test.Hspec.Runner.Tree
import qualified Test.Hspec.Discover as H

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "postProcessSpec" $ do
    it "adds heuristic source locations" $ do
      let c = build $ do
            strlit "foo"
            strlit "bar"
            strlit "baz"

      withFileContent c $ \src -> do
        [Leaf "bar" item] <- toTree . H.postProcessSpec src $ do
          H.it "bar" True
        itemLocation item `shouldBe` Just (Location src 2 0 BestEffort)
  where
    strlit :: String -> Builder
    strlit = fromString . show
