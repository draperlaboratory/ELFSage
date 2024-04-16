import Cli
import ELFSage.Util.Cli

import ELFSage.Types.ELFHeader
import ELFSage.Types.File


def mkRawProgramHeaderErrors
  [ELFHeader α]
  (eh : α)
  (bytes : ByteArray)
  : List (Except String RawProgramHeaderTableEntry) :=
  let shoffsets := (List.range (ELFHeader.e_phnum eh)).map λidx ↦ ELFHeader.e_phoff eh + ELFHeader.e_phentsize eh * idx
  let isBigendian := ELFHeader.isBigendian eh
  let is64Bit := ELFHeader.is64Bit eh
  List.map (λoffset ↦ mkRawProgramHeaderTableEntry? bytes is64Bit isBigendian offset) shoffsets

def mkRawSectionHeaderErrors
  [ELFHeader α]
  (eh : α)
  (bytes : ByteArray)
  : List (Except String RawSectionHeaderTableEntry) :=
  let shoffsets := (List.range (ELFHeader.e_shnum eh)).map λidx ↦ ELFHeader.e_shoff eh + ELFHeader.e_shentsize eh * idx
  let isBigendian := ELFHeader.isBigendian eh
  let is64Bit := ELFHeader.is64Bit eh
  List.map (λoffset ↦ mkRawSectionHeaderTableEntry? bytes is64Bit isBigendian offset) shoffsets

def runValidateCmd (p : Cli.Parsed) : IO UInt32 := do

  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFHeader? bytes with
  | .error warn => do
    IO.println "The ELF Header can't be recovered, so nothing else is readable:"
    IO.println warn *> return 1
  | .ok elfheader => do

  let phtEntries := mkRawProgramHeaderErrors elfheader bytes
  let shtEntries := mkRawSectionHeaderErrors elfheader bytes

  let mut idx := 0

  for ent in phtEntries do
    idx := idx + 1

    match ent with
      | .error s => IO.println s!"Program Header Table Entry {idx}: {s}"
      | .ok _ => pure ()

  idx := 0

  for ent in shtEntries do
    idx := idx + 1
    match ent with
      | .error s => IO.println s!"Section Header Table Entry {idx}: {s}"
      | .ok _ => pure ()

  return 0
