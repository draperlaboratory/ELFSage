import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader

def checkImplemented (p: Cli.Parsed) : Except String Unit := do
  let unimplemented := 
    [ "a", "all"
    , "l", "segments"
    , "S", "section-headers" , "sections"
    , "g", "section-groups"
    , "t", "section-details"
    , "e", "headers"
    , "s", "syms", "symbols"
    , "dyn-syms"
    , "lto-syms"
    , "sym-base"
    , "C", "demangle"
    , "n", "notes"
    , "r", "relocs"
    , "u", "unwind"
    , "d", "dynamic"
    , "V", "version-info"
    , "A", "arch-specific"
    , "c", "archive-index"
    , "D", "use-dynamic"
    , "L", "lint"
    , "x", "hex-dump"
    , "p", "string-dump"
    , "R", "relocated-dump"
    , "z", "decompress"
    ]
  for flag in unimplemented do
    if p.hasFlag flag 
    then throw s!"The flag --{flag} isn't implemented yet, sorry!"

  return ()

def runReadCmd (p: Cli.Parsed): IO UInt32 := do
  
  match checkImplemented p with
  | .error warn => IO.println warn *> return 1
  | .ok _ => do

  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFHeader? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok header => do

  if p.hasFlag "file-header" 
  then IO.println $ repr header

  return 0
