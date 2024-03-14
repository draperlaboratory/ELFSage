import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable

def checkImplemented (p: Cli.Parsed) : Except String Unit := do
  let unimplemented := 
    [ "a", "all"
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
  | .ok elfheader => do

  if p.hasFlag "file-header" 
  then IO.println $ repr elfheader

  if p.hasFlag "program-headers" ∨ p.hasFlag "segments" 
  then for idx in [:elfheader.phnum] do
    IO.println s!"\nProgram Header {idx}\n"
    let offset := elfheader.phoff + (idx * elfheader.phentsize)
    match mkRawProgramHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error warn => IO.println warn
    | .ok programHeader => IO.println $ repr programHeader

  return 0
