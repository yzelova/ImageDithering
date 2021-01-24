# Image Dithering 
## Setup
1. Install the [Word8](https://hackage.haskell.org/package/word8-0.1.3) library (used for processing ByteStrings)
2. `ghc  Main.hs`
3. The supported commands are showed when starting the application.
---
## Tests
> Install the [HUnit](https://hackage.haskell.org/package/HUnit) library
> `ghci Tests\RunTests.hs`
> `main`

Only 1 algorithm is currently being tested, since all algorithms are implemented in relatively the same way. Dithering algorithms have more specific results with bigger Image objects. Calculating 1 algorithm's effect on a given image is a hard task to do by hand, so unit tests would be more prone to have errors themselves, rather than finding ones in the code. In this case, more efficient testing is done by applying the algorithms to images and visualising the results with an image viewer.

---
## Notes
If the functions that implement the algorithms are given a RGB image, they would reduce the color space to an 8bit one. Grayscale and BW images would result in BW images. However, when using the tool from the CLI, providing a RGB image would first convert it to Grayscale and then result in a BW image. 