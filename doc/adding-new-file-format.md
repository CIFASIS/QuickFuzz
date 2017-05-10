# How to add a new file format to generate and mutate in QuickFuzz

First, clone the QuickFuzz repository and make sure it compiles using
the "all" flag. Then, suppose you want to add a new image format called FMT 
in the "image" section.

1. Create the file FMT.hs inside src/Test/QuickFuzz/Gen/Image/
  The module name should be Test.QuickFuzz.Gen.Image.FMT and it should properly
  define arbitrary and show instances of a type representing IMG files.
2. Declare a FormatInfo value named imgInfo:

  ```
  imgInfo :: FormatInfo IMGFile NoActions
  imgInfo = def
    { encode = imgencode
    , random = arbitrary
    , value = show
    , ext = "img"
    }
  ```

3. Declare and export Test.QuickFuzz.Gen.Image.FMT in src/Test/QuickFuzz/Gen/Image.hs
4. Add ("img", 'imgInfo) into the IMAGE part in the formats list in app/Formats.hs
5. Add the necesary modules to compile the FMT module into the IMAGE section in QuickFuzz.cabal
6. Add Test.QuickFuzz.Gen.Image.FMT in the exposed modules into the IMAGE section in QuickFuzz.cabal

