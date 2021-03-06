{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.HUnitSpec (main, spec) where

import           Helper hiding (example)

import           Test.Hspec.Runner.Tree
import           Test.Hspec.HUnit
import           Test.HUnit

main :: IO ()
main = hspec spec

shouldYield :: Test -> [Tree ()] -> Expectation
a `shouldYield` b = map (Blind . (() <$)) <$> toTree (fromHUnitTest a) `shouldReturn` map Blind b

instance Eq a => Eq (Tree a) where
  x == y = case (x, y) of
    (Node s1 xs, Node s2 ys) -> s1 == s2 && xs == ys
    (Leaf s1 a1, Leaf s2 a2) -> s1 == s2 && a1 == a2
    _ -> False

spec :: Spec
spec = do
  describe "fromHUnitTest" $ do
    let e = TestCase $ pure ()

    it "works for a TestCase" $ do
      e `shouldYield` [example "<unlabeled>"]

    it "works for a labeled TestCase" $ do
      TestLabel "foo" e
        `shouldYield` [example "foo"]

    it "works for a TestCase with nested labels" $ do
      (TestLabel "foo" . TestLabel "bar") e
        `shouldYield` [Node "foo" [example "bar"]]

    it "works for a flat TestList" $ do
      TestList [e, e, e]
        `shouldYield` [example "<unlabeled>", example "<unlabeled>", example "<unlabeled>"]

    it "works for a nested TestList" $ do
      (TestLabel "foo" . TestLabel "bar" . TestList) [TestLabel "one" e, TestLabel "two" e, TestLabel "three" e]
        `shouldYield` [Node "foo" [Node "bar" [example "one", example "two", example "three"]]]
  where
    example :: String -> Tree ()
    example r = Leaf r ()
