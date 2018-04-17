module StringSumSpec where

import           Test.Hspec
import           TypesHierarchy

spec :: Spec
spec = do
    it "stringSum" $ do
        stringSum "1 2 3" `shouldBe` Just 6
        stringSum "1 2 \t \n    3" `shouldBe` Just 6
        stringSum "a" `shouldBe` Nothing
        stringSum "1 2 3 a 4" `shouldBe` Nothing
