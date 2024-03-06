import Cli
import ELFSage.Util.Cli

def runHexDumpCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary : System.FilePath := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary
  let mut idx ← pure 0
  for byte in bytes do
    if idx % 8 == 0 then do IO.print " "
    if idx % 16 == 0 then do IO.print "\n"
    IO.print $ (Nat.toDigits 16 (byte.toNat)) 
      |> (λl ↦ if l.length == 1 then '0' :: l else l)
      |> List.asString
    IO.print " "
    idx ← pure $ idx + 1
  IO.print "\n\n"
  return 0
