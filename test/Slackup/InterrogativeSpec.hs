module Slackup.InterrogativeSpec where

import Test.Hspec
import Slackup.Interrogative

examp = Interrogative "Do you like programming?" "Not really"

spec :: Spec
spec = do
  describe "ask" $
    it "creates an interrogative with the question asked" $
      asked examp `shouldBe` "Do you like programming?"
  describe "answer" $
    it "creates an interrogative with the first interrogatives question and the given answer" $ do
      answered examp `shouldBe` "Not really"
