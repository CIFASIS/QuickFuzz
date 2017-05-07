# How to add a new file format to QuickFuzz

Suppose you want to add a new image format called FMT.

1. Create the file FMT.hs inside src/Test/QuickFuzz/Gen/Image/
  a. The module name should be Test.QuickFuzz.Gen.Image.FMT
  b. 

```
imgInfo :: FormatInfo IMG NoActions
imgInfo = def
    { encode = encodeIMG
    , random = arbitrary
    , value = show
    , ext = "img"
    }
```
2. Import and export Test.QuickFuzz.Gen.Image.FMT in src/Test/QuickFuzz/Gen/Image.hs
3. Add ("img", 'imgInfo) into the IMAGE part in the formats list in app/Formats.hs
4. Add Test.QuickFuzz.Gen.Image.FMT in the exposed modules into the IMAGE section in QuickFuzz.cabal
