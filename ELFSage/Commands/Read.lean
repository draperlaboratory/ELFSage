import Cli
import ELFSage.Util.Cli

def runReadCmd (p: Cli.Parsed): IO UInt32 := do
  let unimplemented := 
    [ "a", "all"
    , "file-header"
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
    if p.hasFlag flag then do
      IO.println s!"The flag --{flag} isn't implemented yet, sorry!"
      return 1
  return 0
