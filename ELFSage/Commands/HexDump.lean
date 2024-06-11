import Cli
import ELFSage.Util.Cli
import ELFSage.Util.IO

def runHexDumpCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary : System.FilePath := (p.positionalArg! "targetBinary").as! System.FilePath
  IO.FS.readBinFile targetBinary >>= dumpBytesAsHex
  IO.print "\n\n"
  return 0
