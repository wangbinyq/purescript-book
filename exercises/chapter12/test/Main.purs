module Test.Main where

import Prelude
import Test.Copy (copyFile)
import Test.HTTP (getUrl)
import Test.MySolutions
import Data.Array ((..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Exception (message)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, unlink)
import Node.Path (FilePath)
import Node.Path as Path
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

inDir :: FilePath
inDir = Path.concat [ "test", "data" ]

outDir :: FilePath
outDir = Path.concat [ "test", "data-out" ]

main :: Effect Unit
main =
  runTest do
    test "setup" do
      -- Clear test output directory
      files <- readdir outDir
      for_ files \f -> unlink $ Path.concat [ outDir, f ]
    runChapterExamples
    {-  Move this block comment starting point to enable more tests
-}
    test "concatenateFiles" do
      let
        inFoo = Path.concat [ inDir, "foo.txt" ]

        inBar = Path.concat [ inDir, "bar.txt" ]

        outFooBar = Path.concat [ outDir, "foobar.txt" ]
      concatenateFiles inFoo inBar outFooBar
      -- Check for valid concat
      inFooTxt <- readTextFile UTF8 inFoo
      inBarTxt <- readTextFile UTF8 inBar
      outFooBarTxt <- readTextFile UTF8 outFooBar
      Assert.equal (inFooTxt <> inBarTxt) outFooBarTxt
    test "concatenateMany" do
      let
        inFiles = map (\i -> Path.concat [ inDir, "many", "file" <> show i <> ".txt" ]) $ 1 .. 9

        outFile = Path.concat [ outDir, "manyConcat.txt" ]

        expectedOutFile = Path.concat [ inDir, "manyConcat.txt" ]
      concatenateMany inFiles outFile
      -- Check for valid concat
      actualOutTxt <- readTextFile UTF8 outFile
      expectedOutTxt <- readTextFile UTF8 expectedOutFile
      Assert.equal expectedOutTxt actualOutTxt
    suite "countCharacters" do
      test "exists" do
        chars <- countCharacters $ Path.concat [ inDir, "foo.txt" ]
        Assert.equal (Right 41) $ lmap message chars
      test "missing" do
        chars <- countCharacters $ Path.concat [ inDir, "foof.txt" ]
        Assert.equal (Left "ENOENT: no such file or directory, open 'test/data/foof.txt'") $ lmap message chars
    test "writeGet" do
      let
        outFile = Path.concat [ outDir, "user.txt" ]

        expectedOutFile = Path.concat [ inDir, "user.txt" ]
      writeGet "https://reqres.in/api/users/1" outFile
      -- Check for valid write
      actualOutTxt <- readTextFile UTF8 outFile
      expectedOutTxt <- readTextFile UTF8 expectedOutFile
      Assert.equal expectedOutTxt actualOutTxt
    test "concatenateManyParallel" do
      let
        inFiles = map (\i -> Path.concat [ inDir, "many", "file" <> show i <> ".txt" ]) $ 1 .. 9

        outFile = Path.concat [ outDir, "manyConcatParallel.txt" ]

        expectedOutFile = Path.concat [ inDir, "manyConcat.txt" ]
      concatenateManyParallel inFiles outFile
      -- Check for valid concat
      actualOutTxt <- readTextFile UTF8 outFile
      expectedOutTxt <- readTextFile UTF8 expectedOutFile
      Assert.equal expectedOutTxt actualOutTxt
    suite "getWithTimeout" do
      test "valid site" do
        let
          expectedOutFile = Path.concat [ inDir, "user.txt" ]
        actual <- getWithTimeout 1000.0 "https://reqres.in/api/users/1"
        expected <- Just <$> readTextFile UTF8 expectedOutFile
        Assert.equal expected actual
      test "no response" do
        actual <- getWithTimeout 10.0 "https://example.com:81"
        Assert.equal Nothing actual
    suite "recurseFiles" do
      let
        recurseDir = Path.concat [ inDir, "tree" ]
      test "many files" do
        expectedTxt <- readTextFile UTF8 $ Path.concat [ recurseDir, "expected.txt" ]
        let
          expected = Path.normalize <$> split (Pattern "\n") expectedTxt
        actual <- recurseFiles $ Path.concat [ recurseDir, "root.txt" ]
        let
          actualRelative = map (\f -> Path.relative recurseDir f) actual
        Assert.equal (Set.fromFoldable expected) $ Set.fromFoldable actualRelative
      test "one file" do
        let
          file = Path.concat [ recurseDir, "c/unused.txt" ]

          expected = [ file ]
        actual <- recurseFiles file
        Assert.equal (Set.fromFoldable expected) $ Set.fromFoldable actual

-}
runChapterExamples :: TestSuite
runChapterExamples = do
  test "copyFile" do
    let
      inFoo = Path.concat [ inDir, "foo.txt" ]

      outFoo = Path.concat [ outDir, "foo.txt" ]
    copyFile inFoo outFoo
    -- Check for valid copy
    inFooTxt <- readTextFile UTF8 inFoo
    outFooTxt <- readTextFile UTF8 outFoo
    Assert.equal inFooTxt outFooTxt
  test "getUrl" do
    let
      expectedOutFile = Path.concat [ inDir, "user.txt" ]
    str <- getUrl "https://reqres.in/api/users/1"
    -- Check for valid read
    expectedOutTxt <- readTextFile UTF8 expectedOutFile
    Assert.equal expectedOutTxt str
