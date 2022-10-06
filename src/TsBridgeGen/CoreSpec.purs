module TsBridge.CoreSpec
  ( spec
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Test.QuickCheck.Laws.Data (checkMonoid, checkSemigroup)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import TsBridgeGen (SourcePosition(..))
import TsBridgeGen.Core (indexToSourcePos)
import Type.Proxy (Proxy(..))

spec :: Spec Unit
spec = describe "TsBridge.Core" do
  describe "indexToSourcePos" do
    it "works for index -1" do
      indexToSourcePos (-1) "foo\nbar"
        # shouldEqual
        $ Nothing
    it "works for index 0" do
      indexToSourcePos 0 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 0 })
    it "works for index 1" do
      indexToSourcePos 1 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 1 })
    it "works for index 1" do
      indexToSourcePos 2 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 2 })
    it "works for index 3" do
      indexToSourcePos 3 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 0 })
    it "works for index 4" do
      indexToSourcePos 4 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 1 })
    it "works for index 5" do
      indexToSourcePos 5 "foo\nbar"
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 2 })
    it "works for index 6" do
      indexToSourcePos 6 "foo\nbar"
        # shouldEqual
        $ Nothing
    it "works for index 7" do
      indexToSourcePos 7 "foo\nbar"
        # shouldEqual
        $ Nothing
  describe "SourcePosition" do
    it "has a la abiding Semiring instance" do
      liftEffect $ checkSemigroup (Proxy :: _ SourcePosition)
    it "has a la abiding Monoid instance" do
      liftEffect $ checkMonoid (Proxy :: _ SourcePosition)
