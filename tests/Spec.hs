-- Unit Testing
import Test.Hspec
import qualified Data.ByteString.Lazy as BS
import Testing

main :: IO()
main = hspec $ do
	describe "isSameFile" $ do
  	it "returns True if original text and decrypted text are identical, else False" $ do
		  isSameFile "t1.txt" "decrypted_t1.txt" `shouldReturn` True
	it "returns True if encrypted text and decoded text are identical, else False" $ do
		  isSameFile "encrypted_t1.txt" "decoded_t1.txt" `shouldReturn` True
	it "returns True if original text and decrypted text are identical, else False" $ do
		  isSameFile "t2.txt" "decrypted_t2.txt" `shouldReturn` True
	it "returns True if encrypted text and decoded text are identical, else False" $ do
		  isSameFile "encrypted_t2.txt" "decoded_t2.txt" `shouldReturn` True

	it "returns True if original image and decoded image are identical, else False" $ do
		  isSameFile "p5.png" "decoded_p5.png" `shouldReturn` True
	it "returns True if original image and decoded image are identical, else False" $ do
		  isSameFile "p6.png" "decoded_p6.png" `shouldReturn` True
