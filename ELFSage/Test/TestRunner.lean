import ELFSage.Types.File

private def DATA_DIR_RELATIVE: System.FilePath := System.mkFilePath [ "Data" ]
private def ELF_FILES_DIR_RELATIVE: System.FilePath := DATA_DIR_RELATIVE.join $ System.mkFilePath [ "ELFFiles" ]
private def EXPECTED_OUTPUT_DIR_RELATIVE: System.FilePath := DATA_DIR_RELATIVE.join $ System.mkFilePath [ "PrintedHeaders" ]
private def TEST_OUTPUT_DIR_RELATIVE: System.FilePath := System.mkFilePath [ "TestRunOutput" ]

private def EXAMPLE_TEST_NAME: System.FilePath := System.mkFilePath [ "true" ]

def runSingleTest (elfFile: System.FilePath) (expectedOutputFile: System.FilePath)
    (testOutputFile: System.FilePath): IO UInt32 := do
  -- TODO: Clean this up. This file is being written so that, if it did not previously exist,
  -- deleting it will not error.
  IO.FS.writeFile testOutputFile ""

  -- delete the existing test output file if it exists to avoid debugging confusion
  IO.FS.removeFile testOutputFile

  let bytes ← IO.FS.readBinFile elfFile

  match mkRawELFFile? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok elffile => do

  let expectedString ← IO.FS.readFile expectedOutputFile
  let actualString := RawELFFile.headersToString elffile ++ "\n"

  if expectedString == actualString then
    IO.println "PASS"
    return 0
  else
    IO.println "FAIL"
    IO.FS.writeFile testOutputFile actualString
    IO.println $ s!"Expected {testOutputFile} to equal {expectedOutputFile}."
    return 1

def runTests (testDirRelative : System.FilePath): IO UInt32 := do
  let rootDir ← IO.currentDir
  let testDir: System.FilePath := rootDir.join testDirRelative
  let elfFilesDir := testDir.join ELF_FILES_DIR_RELATIVE
  let testOutputDir := testDir.join TEST_OUTPUT_DIR_RELATIVE
  let expectedOutputDir := testDir.join EXPECTED_OUTPUT_DIR_RELATIVE

  -- for now just run a single example test
  let elfFile := elfFilesDir.join EXAMPLE_TEST_NAME
  let expectedOutputFile := expectedOutputDir.join EXAMPLE_TEST_NAME
  let testOutputFile := testOutputDir.join EXAMPLE_TEST_NAME
  runSingleTest elfFile expectedOutputFile testOutputFile
