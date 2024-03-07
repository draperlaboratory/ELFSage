import «ELFSage»
import Cli

open Cli

def hexDumpCmd : Cmd := `[Cli|
  hexdump VIA runHexDumpCmd; ["0.0.0"]
  "hexdump, but in Lean!"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
]

def viewHeaderCmd : Cmd := `[Cli|
  viewHeader VIA runViewHeaderCmd; ["0.0.0"]
  "view the ELF Header of a given binary"

  ARGS:
      targetBinary : System.FilePath; "The ELF binary to be analyzed"
]

def mainCmd : Cmd := `[Cli|
  elfSage NOOP; ["0.0.0"]
  "An ELF validator"

  SUBCOMMANDS:
      hexDumpCmd;
      viewHeaderCmd
]

def main (args: List String): IO UInt32 :=
  mainCmd.validate args
