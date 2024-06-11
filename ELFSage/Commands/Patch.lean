import Cli
import ELFSage.Util.Cli
import ELFSage.Util.IO
import ELFSage.Types.File

--Get the minimum filesize in bytes required to accommodate ELF data
def RawELFFile.getFileSize (elffile : RawELFFile) : Nat :=

  let elfheader := elffile.getRawELFHeader

  let sec_max := elffile.getRawSectionHeaderTableEntries.foldr
    (λ(shte,_) acc => Nat.max (SectionHeaderTableEntry.sh_end shte) acc) 0

  let seg_max := elffile.getRawProgramHeaderTableEntries.foldr
    (λ(phte,_) acc => Nat.max (ProgramHeaderTableEntry.ph_end phte) acc) 0

  [seg_max, sec_max, ELFHeader.ph_end elfheader, ELFHeader.sh_end elfheader].foldr Nat.max 0

def RawELFFile.serialize (elffile : RawELFFile) : IO ByteArray := do
  let elfheader := elffile.getRawELFHeader
  let elfheader_bytes := ELFHeader.bytes elfheader

  let isBigendian := ELFHeader.isBigendian elfheader

  let section_header_table_entries := elffile.getRawSectionHeaderTableEntries

  let program_header_table_entries := elffile.getRawProgramHeaderTableEntries

  -- We need the full zeroed-out byte array
  -- copySlice grows the target bytearray if needed, but it won't insert the
  -- source array beyond the final byte of the target array.

  let mut outBytes := ByteArray.mkEmpty elffile.getFileSize

  for _ in [:elffile.getFileSize] do
    outBytes := outBytes.push 0

  outBytes := elfheader_bytes.copySlice 0 outBytes 0 (elfheader_bytes.size)

  let mut program_header_base := ELFHeader.e_phoff elfheader

  for phte in program_header_table_entries do
    --copy the header to the header table
    let header_bytes := ProgramHeaderTableEntry.bytes phte.fst isBigendian
    outBytes := header_bytes.copySlice 0 outBytes program_header_base (header_bytes.size)
    program_header_base := program_header_base + ELFHeader.e_phentsize elfheader

    --copy the segment to the appropriate position
    let entryOffset := ProgramHeaderTableEntry.p_offset phte.fst
    let segment_bytes := phte.snd.segment_body
    outBytes := segment_bytes.copySlice 0 outBytes entryOffset segment_bytes.size

  let mut section_header_base := ELFHeader.e_shoff elfheader

  for shte in section_header_table_entries do
    --copy the header to the header table
    let header_bytes := SectionHeaderTableEntry.bytes shte.fst isBigendian
    outBytes := header_bytes.copySlice 0 outBytes section_header_base (header_bytes.size)
    section_header_base := section_header_base + ELFHeader.e_shentsize elfheader

    --copy the section to the appropriate position
    let entryOffset := SectionHeaderTableEntry.sh_offset shte.fst
    let section_bytes := shte.snd.section_body
    outBytes := section_bytes.copySlice 0 outBytes entryOffset section_bytes.size

  return outBytes

def runNoopCmd (p : Cli.Parsed) : IO UInt32 := do
  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let outPath := (p.positionalArg! "outPath").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFFile? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok elffile => do

  let outBytes ← RawELFFile.serialize elffile

  IO.FS.writeBinFile outPath outBytes

  return 0

def runAddSpaceCmd (_ : Cli.Parsed): IO UInt32 := do
  return 1
