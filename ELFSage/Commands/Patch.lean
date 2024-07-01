import Cli
import ELFSage.Util.Cli
import ELFSage.Util.IO
import ELFSage.Types.File
import ELFSage.Types.Relocation
import ELFSage.Constants.SectionHeaderTable
import ELFSage.Commands.Read

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

  let mut program_header_base := ELFHeader.e_phoff elfheader

  let mut section_header_base := ELFHeader.e_shoff elfheader


  for phte in program_header_table_entries do
    --copy the header to the header table
    let header_bytes := ProgramHeaderTableEntry.bytes phte.fst isBigendian
    outBytes := header_bytes.copySlice 0 outBytes program_header_base (header_bytes.size)
    program_header_base := program_header_base + ELFHeader.e_phentsize elfheader

    --copy the segment to the appropriate position
    let entryOffset := ProgramHeaderTableEntry.p_offset phte.fst
    let segment_bytes := phte.snd.segment_body
    outBytes := segment_bytes.copySlice 0 outBytes entryOffset segment_bytes.size

  for shte in section_header_table_entries do
    --copy the header to the header table
    let header_bytes := SectionHeaderTableEntry.bytes shte.fst isBigendian
    outBytes := header_bytes.copySlice 0 outBytes section_header_base (header_bytes.size)
    section_header_base := section_header_base + ELFHeader.e_shentsize elfheader

    --copy the section to the appropriate position
    let entryOffset := SectionHeaderTableEntry.sh_offset shte.fst
    let section_bytes := shte.snd.section_body
    outBytes := section_bytes.copySlice 0 outBytes entryOffset section_bytes.size

  outBytes := elfheader_bytes.copySlice 0 outBytes 0 (elfheader_bytes.size)

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


def nullSegment32 : ELF32ProgramHeaderTableEntry :=
  {
    p_type   := 0,
    p_flags  := 0,
    p_offset := 0,
    p_vaddr  := 0,
    p_paddr  := 0,
    p_filesz := 0,
    p_memsz  := 0,
    p_align  := 0,
  }

def nullSegment64 : ELF64ProgramHeaderTableEntry :=
  {
    p_type   := 0,
    p_flags  := 0,
    p_offset := 0,
    p_vaddr  := 0,
    p_paddr  := 0,
    p_filesz := 0,
    p_memsz  := 0,
    p_align  := 0,
  }

def nullInterpretedSegment : InterpretedSegment := {
  segment_type   := 0,
  segment_size   := 0,
  segment_memsz  := 0,
  segment_base   := 0,
  segment_paddr  := 0,
  segment_align  := 0,
  segment_offset := 0,
  segment_body   := ByteArray.mk #[]
  segment_flags  := (False,False,False)
}

def RawELFFile.addDummyProgramHeader (elffile : RawELFFile) : RawELFFile :=
  match elffile with
  | .elf32 elffile =>
    let new_segments := elffile.interpreted_segments.cons (nullSegment32, nullInterpretedSegment)
    let new_header := { elffile.file_header with e_phnum := elffile.file_header.e_phnum + 1 }
    .elf32 { elffile with
      interpreted_segments := new_segments
      file_header := new_header
    }
  | .elf64 elffile =>
    let new_segments := elffile.interpreted_segments.cons (nullSegment64, nullInterpretedSegment)
    let new_header := { elffile.file_header with e_phnum := elffile.file_header.e_phnum + 1 }
    .elf64 { elffile with
      interpreted_segments := new_segments
      file_header := new_header
    }

def shift_rela_64 (shift : elf64_off → elf64_off) (isBigendian : Bool) (bytes : ByteArray) : ByteArray :=
  let split_bytes := (List.range (bytes.size / 0x18)).map $ λn ↦ bytes.extract (n * 0x18) ((n + 1) * 0x18)
  let make_mreloca (b : ByteArray) := if h: b.size ≥ 0x18 then Option.some $ mkELF64RelocationA isBigendian b 0 h else .none
  let mrelocs := split_bytes.map make_mreloca
  let mrelocs_shifted := mrelocs.map λmrela ↦ match mrela with
    | .none => Option.none
    | .some rela => .some { rela with ra_offset := shift rela.ra_offset }
  mrelocs_shifted.foldr λmrela acc ↦ match mrela with
    | .none => (List.replicate 0x18 0).toByteArray ++ acc
    | .some rela => rela.bytes isBigendian ++ acc
    ByteArray.empty

def shift_rel_64 (shift : elf64_off → elf64_off) (isBigendian : Bool) (bytes : ByteArray) : ByteArray :=
  let split_bytes := (List.range (bytes.size / 0x10)).map $ λn ↦ bytes.extract (n * 0x10) ((n + 1) * 0x10)
  let make_mreloca (b : ByteArray) := if h: b.size ≥ 0x10 then Option.some $ mkELF64Relocation isBigendian b 0 h else .none
  let mrelocs := split_bytes.map make_mreloca
  let mrelocs_shifted := mrelocs.map λmrela ↦ match mrela with
    | .none => Option.none
    | .some rel => .some { rel with r_offset := shift rel.r_offset }
  mrelocs_shifted.foldr λmrela acc ↦ match mrela with
    | .none => (List.replicate 0x10 0).toByteArray ++ acc
    | .some rel => rel.bytes isBigendian ++ acc
    ByteArray.empty

def RawELFFile.expandPHDRSegment (elffile : RawELFFile) : Option RawELFFile :=
  let isBigendian := isBigendian elffile
  match elffile with
  | .elf64 elffile =>
    match elffile.interpreted_segments.find? λ(phte,_) => phte.p_type == 6 with --PHDR Segment
    | .none => .none
    | .some (phdr_phte, _) =>
    let contains_phdr (phte : ELF64ProgramHeaderTableEntry) :=
      phte.p_offset ≤ phdr_phte.p_offset &&
      phte.p_offset + phte.p_filesz > phdr_phte.p_offset &&
      phte.p_type == 1 --PT_LOAD

    let shift_if_after_phdr n := if n > phdr_phte.p_offset then n + 0x1000 else n

    -- we rewrite the ELFSage representation of the program header table
    let new_segments := elffile.interpreted_segments.map λ(phte, seg) =>
      if contains_phdr phte || phte.p_type = 6
      then ({ phte with
              p_filesz := phte.p_filesz + 0x1000,
              p_memsz := phte.p_memsz + 0x1000
            }, seg)
      else ({ phte with
              p_offset := shift_if_after_phdr phte.p_offset,
              p_vaddr := shift_if_after_phdr phte.p_vaddr,
              p_paddr := shift_if_after_phdr phte.p_paddr,
            }, seg)


    -- We generate a new set of contents for the program header table based on
    -- our rewritten table
    let zeroes := (List.replicate 0x1000 0).toByteArray
    let new_segment_bytes := new_segments.foldr (λ(phte,_) acc => phte.bytes isBigendian ++ acc) zeroes

    -- And we rewrite the byte contents of the PHDR segment and the containing
    -- load segment based on that. Also the dynamic section
    let new_segments := new_segments.map λ(phte, seg) =>
      if phte.p_type = 6 -- PHDR segment
      then (phte, { seg with segment_body := new_segment_bytes })
      else if contains_phdr phte --Containing Load segment
      then
        let dest_offset := phdr_phte.p_offset - phte.p_offset
        -- We zero-extend the original load segment
        let expanded_segment := seg.segment_body ++ zeroes
        --we then shift the bytes of the extended load segment below the insertion
        --point to the right by 0x1000 to make room for the header table
        let shifted_body := seg.segment_body.copySlice
          dest_offset.toNat
          expanded_segment
          (dest_offset.toNat + 0x1000)
          (seg.segment_body.size - dest_offset.toNat)
        --then we insert the header table
        let new_body := new_segment_bytes.copySlice 0 shifted_body dest_offset.toNat new_segment_bytes.size
        (phte, { seg with segment_body := new_body })
      else (phte, seg)

    -- finally, we're going to want to rewrite the dynamic and relocation sections.
    let new_sections := elffile.interpreted_sections.map λ(shte, sec) =>
      let new_shte := { shte with
        sh_addr := shift_if_after_phdr shte.sh_addr
        sh_offset := shift_if_after_phdr shte.sh_offset
      }
      if shte.sh_type.toNat == ELFSectionHeaderTableEntry.Type.SHT_RELA then
        (new_shte, { sec with section_body := shift_rela_64 shift_if_after_phdr isBigendian sec.section_body})
      else if shte.sh_type.toNat == ELFSectionHeaderTableEntry.Type.SHT_REL then
        (new_shte, { sec with section_body := shift_rel_64 shift_if_after_phdr isBigendian sec.section_body})
      else if shte.sh_type.toNat == ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC then
        (new_shte, sec) --todo
      else
      (new_shte, sec)

    let new_header := { elffile.file_header with
      e_entry := shift_if_after_phdr elffile.file_header.e_entry
      e_shoff := shift_if_after_phdr elffile.file_header.e_shoff
    }

    .some $ .elf64 { elffile with
      interpreted_segments := new_segments
      interpreted_sections := new_sections
      file_header := new_header
    }
  | .elf32 _ => .none

def runAddSpaceCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let outPath := (p.positionalArg! "outPath").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFFile? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok elffile => do

  match elffile.expandPHDRSegment with
  | .none => IO.println "target binary appears to not contain a program header table" *> return 1
  | .some newFile => do

  let outBytes ← RawELFFile.serialize newFile

  IO.FS.writeBinFile outPath outBytes

  return 0
