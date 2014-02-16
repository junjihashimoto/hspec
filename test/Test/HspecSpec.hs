module Test.HspecSpec (main, spec) where

import           Helper
import           Data.IORef
import           Data.List (isPrefixOf)

import           Test.Hspec.Core (Item(..), Result(..))
import qualified Test.Hspec as H
import qualified Test.Hspec.Runner as H (hspecResult)

import           Test.Hspec.Runner.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "pending" $ do
    it "specifies a pending example" $ do
      r <- runSpec $ do
        H.it "foo" H.pending
      r `shouldSatisfy` any (== "     # PENDING: No reason given")

  describe "pendingWith" $ do
    it "specifies a pending example with a reason for why it's pending" $ do
      r <- runSpec $ do
        H.it "foo" $ do
          H.pendingWith "for some reason"
      r `shouldSatisfy` any (== "     # PENDING: for some reason")

  describe "describe" $ do
    let testSpec = do
          H.describe "some subject" $ do
            H.it "foo" True
            H.it "bar" True
            H.it "baz" True
    it "takes a description of what the behavior is for" $ do
      r <- runSpec testSpec
      r `shouldSatisfy` any (== "some subject")

    it "groups behaviors for what's being described" $ do
      r <- filter (isPrefixOf "  - ") `fmap` runSpec testSpec
      length r `shouldBe` 3

    it "can be nested" $ do
      [Node foo [Node bar [Leaf baz _]]] <- toTree $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" True
      (foo, bar, baz) `shouldBe` ("foo", "bar", "baz")

    context "when no description is given" $ do
      it "uses a default description" $ do
        [Node d _] <- toTree (H.describe "" (pure ()))
        d `shouldBe` "(no description given)"

  describe "it" $ do
    it "takes a description of a desired behavior" $ do
      [Leaf requirement _] <- toTree (H.it "whatever" True)
      requirement `shouldBe` "whatever"

    it "takes an example of that behavior" $ do
      [Leaf _ item] <- toTree (H.it "whatever" True)
      itemExample item defaultParams ($ ()) noOpProgressCallback `shouldReturn` Success

    context "when no description is given" $ do
      it "uses a default description" $ do
        [Leaf requirement _] <- toTree (H.it "" True)
        requirement `shouldBe` "(unspecified behavior)"

  describe "example" $ do
    it "fixes the type of an expectation" $ do
      r <- runSpec $ do
        H.it "foo" $ H.example $ do
          pure ()
      r `shouldSatisfy` any (== "1 example, 0 failures")

  describe "parallel" $ do
    it "marks examples for parallel execution" $ do
      [Leaf _ item] <- toTree . H.parallel $ H.it "whatever" True
      itemIsParallelizable item `shouldBe` True

    it "is applied recursively" $ do
      [Node _ [Node _ [Leaf _ item]]] <- toTree . H.parallel $ do
        H.describe "foo" $ do
          H.describe "bar" $ do
            H.it "baz" True
      itemIsParallelizable item `shouldBe` True

  describe "before" $ do
    it "runs an action before every spec item" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.before (append "before") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "before"
        , "foo"
        , "before"
        , "bar"
        ]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        ref <- newIORef ([] :: [String])
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.before (append "before outer") $  H.before (append "before inner") $ do
          H.it "foo" $ do
            append "foo"
        readIORef ref `shouldReturn` [
            "before outer"
          , "before inner"
          , "foo"
          ]

    context "when used with an action that returns a value" $ do
      it "passes that value to the spec item" $ do
        property $ \n -> do
          silence $ H.hspec $ H.before (return n) $ do
            H.it "foo" $ \m -> do
              m `shouldBe` (n :: Int)

    context "when used with a QuickCheck property" $ do
      it "runs action before every check of the property" $ do
        ref <- newIORef ([] :: [String])
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.before (append "before") $ do
          H.it "foo" $ property $ append "foo"
        readIORef ref `shouldReturn` (take 200 . cycle) ["before", "foo"]

  describe "beforeWith" $ do
    it "runs an action before every spec item" $ do
      let action :: Int -> IO String
          action n = return (show n)
      property $ \n -> do
        silence $ H.hspec $ H.before (return n) $ H.beforeWith action $ do
          H.it "foo" $ (`shouldBe` show n)

  describe "beforeAll" $ do
    it "runs an action before the first spec item" $ do
      ref <- newIORef []
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.beforeAll (append "beforeAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "beforeAll"
        , "foo"
        , "bar"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        ref <- newIORef []
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.beforeAll (append "beforeAll") $ do
          return ()
        readIORef ref `shouldReturn` []

    context "when used with an action that returns a value" $ do
      it "passes that value to the spec item" $ do
        property $ \n -> do
          silence $ H.hspec $ H.beforeAll (return n) $ do
            H.it "foo" $ \m -> do
              m `shouldBe` (n :: Int)

  describe "beforeAllWith" $ do
    it "runs an action before the first spec item" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.before (return 23) $ H.beforeAllWith (\n -> append "beforeAllWith" >> return (succ n :: Int)) $ do
        H.it "foo" $ \n -> do
          append ("foo " ++ show n)
        H.it "bar" $ \n -> do
          append ("bar " ++ show n)
      readIORef ref `shouldReturn` [
          "beforeAllWith"
        , "foo 24"
        , "bar 24"
        ]

  describe "after" $ do
    it "must be used with a before action" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.before (return "from before") $ H.after append $ do
        H.it "foo" $ \_ -> do
          append "foo"
      readIORef ref `shouldReturn` [
          "foo"
        , "from before"
        ]

  describe "after_" $ do
    it "runs an action after every spec item" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.after_ (append "after") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "foo"
        , "after"
        , "bar"
        , "after"
        ]

    it "guarantees that action is run" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
      silence . ignoreExitCode $ H.hspec $ H.after_ (append "after") $ do
        H.it "foo" $ do
          ioError $ userError "foo" :: IO ()
          append "foo"
      readIORef ref `shouldReturn` ["after"]

    context "when used multiple times" $ do
      it "is evaluated inside out" $ do
        ref <- newIORef ([] :: [String])
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.after_ (append "after outer") $  H.after_ (append "after inner") $ do
          H.it "foo" $ do
            append "foo"
        readIORef ref `shouldReturn` [
            "foo"
          , "after inner"
          , "after outer"
          ]

  describe "afterAll" $ do
    it "runs an action after the last spec item" $ do
      ref <- newIORef []
      let append n = modifyIORef ref (++ return n)
      silence $ H.hspec $ H.afterAll (append "afterAll") $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "foo"
        , "bar"
        , "afterAll"
        ]

    context "when used with an empty list of examples" $ do
      it "does not run specified action" $ do
        ref <- newIORef []
        let append n = modifyIORef ref (++ return n)
        silence $ H.hspec $ H.afterAll (append "afterAll") $ do
          return ()
        readIORef ref `shouldReturn` []

    context "when action throws an exception" $ do
      it "reports a failure" $ do
        r <- runSpec $ do
          H.afterAll throwException $ do
            H.it "foo" True
        r `shouldSatisfy` any (== "- afterAll-hook FAILED [1]")

  describe "around" $ do
    it "wraps every spec item with an action" $ do
      property $ \n -> do
        silence $ H.hspec $ H.around ($ n) $ do
          H.it "foo" $ \m -> do
            m `shouldBe` (n :: Int)

  describe "around_" $ do
    it "wraps every spec item with an action" $ do
      ref <- newIORef ([] :: [String])
      let append n = modifyIORef ref (++ return n)
          action e = append "before" >> e >> append "after"
      silence $ H.hspec $ H.around_ action $ do
        H.it "foo" $ do
          append "foo"
        H.it "bar" $ do
          append "bar"
      readIORef ref `shouldReturn` [
          "before"
        , "foo"
        , "after"
        , "before"
        , "bar"
        , "after"
        ]

    context "when used multiple times" $ do
      it "is evaluated outside in" $ do
        ref <- newIORef ([] :: [String])
        let append n = modifyIORef ref (++ return n)
            actionOuter e = append "before outer" >> e >> append "after outer"
            actionInner e = append "before inner" >> e >> append "after inner"
        silence $ H.hspec $ H.around_ actionOuter $ H.around_ actionInner $ do
          H.it "foo" $ do
            append "foo"
        readIORef ref `shouldReturn` [
            "before outer"
          , "before inner"
          , "foo"
          , "after inner"
          , "after outer"
          ]

  describe "aroundWith" $ do
    it "wraps every spec item with an action" $ do
      let action :: H.ActionWith String -> H.ActionWith Int
          action e n = e (show n)
      property $ \n -> do
        silence $ H.hspec $ H.before (return n) $ H.aroundWith action $ do
          H.it "foo" $ (`shouldBe` show n)
  where
    runSpec :: H.Spec -> IO [String]
    runSpec = captureLines . H.hspecResult
