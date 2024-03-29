import «ELFSage».Test.TestRunner

def main (_: List String): IO UInt32 :=
  let testDirRelative: System.FilePath := System.mkFilePath [ "ELFSage", "Test" ]
  -- TODO: pass the absolute path instead of the relative path
  -- let rootDir ← IO.currentDir
  -- let testDir: System.FilePath := rootDir.join testDirRelative
  runTests testDirRelative
