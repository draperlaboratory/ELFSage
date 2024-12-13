import «ELFSage»
import Cli

open Cli

def getHelp (p : Cli.Parsed): IO UInt32 := do
  p.printHelp
  return 0

def hexDumpCmd : Cmd := `[Cli|
  hexdump VIA runHexDumpCmd; ["0.0.0"]
  "hexdump, but in Lean!"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
]

def validateCmd: Cmd := `[Cli|
  validate VIA runValidateCmd; ["0.0.0"]
  "Check an ELF binary for structural problems"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
]

/- Some major missing features:

* DWARF debug information, CTF info
* string-dump/hex-dump don't take section names yet

-/

/-- an incomplete readelf clone interface  -/
def readCmd : Cmd := `[Cli|
  read VIA runReadCmd; ["0.0.0"]
  "Display information about the contents of ELF format files"

  FLAGS:
    a, all;                     "Equivalent to: --file-header -l -S -s -r -d -V -A"
    "file-header";              "Display the ELF file header"
    l, "program-headers";       "Display the program headers"
    segments;                   "An alias for --program-headers"
    S, "section-headers";       "Display the sections' header"
    sections;                   "An alias for --section-headers"
    g, "section-groups";        "Display the section groups"
    t, "section-details";       "Display the section details"
    e, "headers";               "Equivalent to: --file-header -l -S"
    s, "syms";                  "Display the symbol table"
    symbols;                    "An alias for --syms"
    "dyn-syms";                 "Display the dynamic symbol table"
    "lto-syms";                 "Display LTO symbol tables"
    "sym-base";                 "--sym-base=[0|8|10|16]. Force base for symbol sizes." ++
                                "The options are  mixed (the default), octal, decimal, hexadecimal."
    C,  "demangle";             "--demangle [STYLE]. Decode mangled/processed symbol names"
    n,  "notes";                "Display the core notes (if present)"
    r,  "relocs";               "Display the relocations (if present)"
    u,  "unwind";               "Display the unwind info (if present)"
    d,  "dynamic";              "Display the dynamic section (if present)"
    V,  "version-info";         "Display the version sections (if present)"
    A,  "arch-specific";        "Display architecture specific information (if any)"
    c,  "archive-index";        "Display the symbol/file index in an archive"
    D,  "use-dynamic";          "Use the dynamic section info when displaying symbols"
    L,  "lint";                 "Display warning messages for possible problems"
    x,  "hex-dump" : String;       "hex-dump=<number|name>. " ++
                                "Dump the contents of section <number|name> as bytes"
    p,  "string-dump" : Nat;    "--string-dump=<number|name>. " ++
                                "Dump the contents of section <number|name> as strings"
    "sym-dump" : Nat;           "sym-dump=<number|name>. " ++
                                "Dump the bytes designated by symbol <number|name>"
    R,  "relocated-dump";       "--relocated-dump=<number|name>" ++
                                "Dump the relocated contents of section <number|name>"
    z,   "decompress";          "Decompress section before dumping it"

  ARGS:
      targetBinary : System.FilePath; "elf-file"
]

def addSpaceCmd : Cli.Cmd := `[Cli|
  addSpace VIA runAddSpaceCmd; ["0.0.0"]
  "Add new program header table entries"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
      outPath : System.FilePath; "The path for the resulting modified binary"
      count : Nat; "The number of entries to add"
]

def noopCmd : Cli.Cmd := `[Cli|
  noop VIA runNoopCmd; ["0.0.0"]
  "Add new program header table entries"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
      outPath : System.FilePath; "The path for the resulting modified binary"
]

def patchCmd : Cmd := `[Cli|
  patch VIA getHelp; ["0.0.0"]
  "Apply some transformation to an ELF file"

  SUBCOMMANDS:
      noopCmd;
      addSpaceCmd
]

def mainCmd : Cmd := `[Cli|
  elfSage VIA getHelp; ["0.0.0"]
  "An ELF validator"

  SUBCOMMANDS:
      hexDumpCmd;
      readCmd;
      patchCmd;
      validateCmd
]

def main (args: List String): IO UInt32 :=
  mainCmd.validate args
