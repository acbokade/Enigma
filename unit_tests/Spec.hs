-- Unit Testing
import ModularArithmetics
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Testing

main :: IO()
main = hspec $ do
	describe "isSameFile" $ do
  	it "returns True if images are identical, else False" $ do
		  isSameFile "pic1.png" "pic1_copy.png" `shouldReturn` True

	it "returns True if images are identical, else False" $ do
		  isSameFile "pic1.png" "pic3.png" `shouldReturn` False 

	it "returns True if texts are identical, else False" $ do
		  isSameFile "test.txt" "test_copy.txt" `shouldReturn` True

	it "returns True if texts are identical, else False" $ do
		  isSameFile "test.txt" "test1.txt" `shouldReturn` False  

	describe "inverseMod" $ do 
	it "returns Modular inverse of integers" $ do
		inverseMod 67 43 `shouldBe` 9
	it "returns Modular inverse of integers" $ do
		inverseMod 47 97 `shouldBe` 64 
	it "returns Modular inverse of integers" $ do
		inverseMod 543 323 `shouldBe` 254
	it "returns Modular inverse of integers" $ do
		inverseMod 312 97 `shouldBe` 37 
	it "returns Modular inverse of integers" $ do
		inverseMod 31 68 `shouldBe` 11


	describe "euclid" $ do
	it "returns coefficients according to extended euclidean theorem " $ do
		euclid 180 150 `shouldBe` (1,-1)	
	it "returns coefficients according to extended euclidean theorem " $ do
 		euclid 45 86  `shouldBe` (-21,11)
 	it "returns coefficients according to extended euclidean theorem " $ do
 		euclid 87 59 `shouldBe` (19,-28)
 	it "returns coefficients according to extended euclidean theorem " $ do
 		euclid 643 134 `shouldBe` (-5,24)
 	it "returns coefficients according to extended euclidean theorem " $ do
 		euclid 466 123 `shouldBe` (52,-197)

 	describe "powerMod" $ do 
 	it "returns base^exponent mod moduli" $ do
 		powerMod 34 23 12 `shouldBe` 4
 	it "returns base^exponent mod moduli" $ do
 		powerMod 324 23 31 `shouldBe` 18
 	it "returns base^exponent mod moduli" $ do
 		powerMod 67 17 17 `shouldBe` 16
 	it "returns base^exponent mod moduli" $ do
 		powerMod 101 37 69 `shouldBe` 29
 	it "returns base^exponent mod moduli" $ do
		powerMod 79 45 15 `shouldBe` 4