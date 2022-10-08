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
      indexToSourcePos "foo\nbar" (-1)
        # shouldEqual
        $ Nothing
    it "works for index 0" do
      indexToSourcePos "foo\nbar" 0
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 0 })
    it "works for index 1" do
      indexToSourcePos "foo\nbar" 1
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 1 })
    it "works for index 2" do
      indexToSourcePos "foo\nbar" 2
        # shouldEqual
        $ (Just $ SourcePosition { line: 0, column: 2 })
    it "works for index 3" do
      indexToSourcePos "foo\nbar" 3
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 0 })
    it "works for index 4" do
      indexToSourcePos "foo\nbar" 4
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 1 })
    it "works for index 5" do
      indexToSourcePos "foo\nbar" 5
        # shouldEqual
        $ (Just $ SourcePosition { line: 1, column: 2 })
    it "works for index 6" do
      indexToSourcePos "foo\nbar" 6
        # shouldEqual
        $ Nothing
    it "works for index 7" do
      indexToSourcePos "foo\nbar" 7
        # shouldEqual
        $ Nothing
  describe "SourcePosition" do
    it "has a la abiding Semiring instance" do
      liftEffect $ checkSemigroup (Proxy :: _ SourcePosition)
    it "has a la abiding Monoid instance" do
      liftEffect $ checkMonoid (Proxy :: _ SourcePosition)
