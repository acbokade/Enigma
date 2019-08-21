[![CircleCI](https://circleci.com/gh/IITH-SBJoshi/haskell-4/tree/master.svg?style=svg&circle-token=6ea8fa40dfb20db60d593afda70f341677cde78f)](https://circleci.com/gh/IITH-SBJoshi/haskell-4/tree/master)

# haskell-4
## Enigma (Image Steganography with Encryption)
This project is built purely in haskell language and is based on Image Steganography . Any file (text or image) can be hidden inside a image using LSB (Least significant bit) algorithm where some number (depending upon size of file) of last bits of each color channel of pixel is modified. In case of text file, first the text file is encrypted using RSA algorithm and then it is hidden inside image using LSB. To obtain hidden text file, first the file is decoded using LSB algorithm and then decrypted using RSA algorithm.

## Installation
Clone the repository

Installing Cabal
```
$ sudo add-apt-repository -y ppa:hvr/ghc
$ sudo apt-get update 
$ sudo apt-get install -y cabal-install
$ sudo cabal update
```
Installing ghc
```
$ sudo apt-get install -y ghc
```
### Building using Cabal
```
$ cabal install 
```
Running
```
$ cd dist/build/haskell4
$ ./haskell4 
```

### Alternative (in case Cabal doesn't work)
In case, above doesn't work then install the required libraries using cabal and 
then run following commands
```
$ cd haskell-4
$ ./build.sh
$ ./Enigma
```

### Options (Arguments to be provided to executable)
For generating keys
```
$ ./haskell4 -k [name of owner of key]
```
For hiding (encrypt and encode) text file inside image
```
$ ./haskell4 -et [original_image] [file_to_hide] [name_of_recipient] [output_image] 
```
For obtaining hidden text file (decode and decrpyt)
```
$ ./haskell4 -dt [encoded_image_name] [private_key_owner] [decrypted_file_name]
```
For hiding (encoding) image inside image
```
$ ./haskell4 -enc [original_image] [image_to_hide] [output_image]
```
For obtaining hidden image (decoding)
```
$ ./haskell4 -dec [encoded_image] [output_image]
```

## Contributors
- Ajinkya Bokade
- Shivashish Suman
- Tungadri Mandal
- Niraj Kamble

## Acknowledgements
Links to images used in tests licensed under CC0 Public Domain, Pixabay License, Creative Commons Attribution, Wikimedia Commons (free for personal use)
- https://pxhere.com/en/photo/295771
- https://pxhere.com/en/photo/345285
- https://pixabay.com/vectors/santa-claus-christmas-reindeer-31665/
- https://pxhere.com/en/photo/402144
- https://pxhere.com/en/photo/1566391
- https://www.deviantart.com/nova-images/art/neon-heart-113987783
- https://pxhere.com/en/photo/467429
- https://en.wikipedia.org/wiki/File:Victoria_Inner_Harbour_HDR.png
- https://en.wikipedia.org/wiki/File:India_satellite_image.png
- https://pixabay.com/illustrations/rose-pink-flower-romance-love-1891108/

## License
This project is provided under MIT license.
