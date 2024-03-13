import «ELFSage»
import Cli

open Cli

def hexDumpCmd : Cmd := `[Cli|
  hexdump VIA runHexDumpCmd; ["0.0.0"]
  "hexdump, but in Lean!"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
]

/- Some major missing flags: DWARF debug information, CTF info -/

/-- an incomplete readelf clone inferface  -/
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
    x,  "hex-dump";             "hex-dump=<number|name>. " ++
                                "Dump the contents of section <number|name> as bytes"
    p,  "string-dump";          "--string-dump=<number|name>. " ++
                                "Dump the contents of section <number|name> as strings"
    R,  "relocated-dump";       "--relocated-dump=<number|name>" ++
                                "Dump the relocated contents of section <number|name>"
    z,   "decompress";          "Decompress section before dumping it"

  ARGS:
      targetBinary : System.FilePath; "elf-file"
]

def mainCmd : Cmd := `[Cli|
  elfSage NOOP; ["0.0.0"]
  "An ELF validator"

  SUBCOMMANDS:
      hexDumpCmd;
      readCmd
]

def main (args: List String): IO UInt32 :=
  mainCmd.validate args
