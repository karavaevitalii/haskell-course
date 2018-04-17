module ParserSpec where

import           Data.Maybe        (isJust, isNothing)
import           ParserCombinators
import           Parsers
import           Test.Hspec

spec :: Spec
spec = do
  it "ok" $ do
    runParser ok "" `shouldSatisfy` isJust
    runParser ok "saf43tw3 rth w4 4" `shouldSatisfy` isJust

  it "eof" $ do
    runParser eof "" `shouldSatisfy` isJust
    runParser eof "f" `shouldSatisfy` isNothing

  it "satisfy" $ do
    runParser (satisfy (== 'a')) "a" `shouldSatisfy` isJust
    runParser (satisfy (== 'a')) "aa" `shouldBe` Just ('a', "a")
    runParser (satisfy (== 'a')) "b" `shouldSatisfy` isNothing

  it "element" $ do
    runParser (element 'a') "abc" `shouldSatisfy` isJust
    runParser (element 'a') "abc" `shouldBe` Just ('a', "bc")
    runParser (element 'a') "bc" `shouldSatisfy` isNothing

  it "stream" $ do
    runParser (stream "ab") "abc" `shouldSatisfy` isJust
    runParser (stream "ab") "abc" `shouldBe` Just ("ab", "c")
    runParser (stream "ab") "bcd" `shouldSatisfy` isNothing

  it "parseCBS" $ do
    runParser parseCBS "" `shouldSatisfy` isJust
    runParser parseCBS "()" `shouldSatisfy` isJust
    runParser parseCBS "(())()" `shouldSatisfy` isJust
    runParser parseCBS ")" `shouldSatisfy` isNothing
    runParser parseCBS "())" `shouldSatisfy` isNothing
    runParser parseCBS "(())" `shouldBe` Just("(())", "")

  it "parseInteger" $ do
    runParser parseInteger "-42" `shouldSatisfy` isJust
    runParser parseInteger "42" `shouldSatisfy` isJust
    runParser parseInteger "+42" `shouldSatisfy` isJust
    runParser parseInteger "cc" `shouldSatisfy` isNothing
    runParser parseInteger "-v" `shouldSatisfy` isNothing 
    runParser parseInteger "-42" `shouldBe` Just (-42, "")
