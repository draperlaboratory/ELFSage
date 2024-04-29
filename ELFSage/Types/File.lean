import ELFSage.Types.ELFHeader
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.Section
import ELFSage.Types.Segment
import ELFSage.Util.Flags
import ELFSage.Util.Hex
import ELFSage.Util.List

open Hex
open Flags

def getInhabitedRanges
  [ELFHeader α] (eh : α)
  [SectionHeaderTableEntry β] (sht : List β)
  [ProgramHeaderTableEntry γ] (pht : List γ)
  : List (Nat × Nat) :=
    (0, ELFHeader.e_ehsize eh) --ELF header
    :: (ELFHeader.e_phoff eh, ELFHeader.e_phoff eh + (ELFHeader.e_phnum eh * ELFHeader.e_phentsize eh)) --Program header entries
    :: (ELFHeader.e_shoff eh, ELFHeader.e_shoff eh + (ELFHeader.e_shnum eh * ELFHeader.e_shentsize eh)) --Section header entries
    :: pht.map (λphte ↦ (ProgramHeaderTableEntry.p_offset phte, ProgramHeaderTableEntry.p_offset phte + ProgramHeaderTableEntry.p_filesz phte))  --segments
    ++ sht.map (λshte ↦ (SectionHeaderTableEntry.sh_offset shte, SectionHeaderTableEntry.sh_offset shte + SectionHeaderTableEntry.sh_size shte)) --sections

def getBitsAndBobs
  [ELFHeader α] (eh : α)
  [SectionHeaderTableEntry β] (sht : List β)
  [ProgramHeaderTableEntry γ] (pht : List γ)
  (bytes : ByteArray)
  : List (Nat × ByteArray) :=
  let ranges := getInhabitedRanges eh sht pht
  let sorted := (ranges.toArray.qsort (λr1 r2 ↦ r1.fst < r2.fst)).toList
  let gaps := toGaps sorted
  gaps.map λgap ↦ (gap.fst, bytes.extract gap.fst gap.snd)
  --toGaps takes a sorted list of pairs of the form (offset, offset+size) and
  --returns the gaps. So for example if the As represent the
  --in-range bytes:
  --
  -- AAABBBAAABBB
  --
  --Then the ranges would be (0,3),(6,9), and the gaps would be (3,6) (9,12)
  where toGaps : List (Nat × Nat) → List (Nat × Nat)
  | [] => []
  | [h] => if h.snd < bytes.size then [(h.snd , bytes.size)] else []
  | h₁ :: h₂ :: t =>
    if h₁.fst = h₂.fst
    then if h₁.snd < h₂.snd
      then toGaps (h₂ :: t)
      else toGaps (h₁ :: t)
    else if h₁.snd < h₂.fst
      then (h₁.snd, h₂.fst) :: toGaps (h₂ :: t)
      else if (h₁.snd < h₂.snd)
        then toGaps (h₂ :: t)
        else toGaps (h₁ :: t)

-- NOTE: this diverges from the LinkSem implementation by using
-- `interpreted_segments` of type ProgramHeaderEntry × InterpretedSegment (and
-- similarly for sections) rather than having separate fields and asking
-- that they have the same length as an invariant.

structure ELF32File where
  /-- The ELF Header -/
  file_header           : ELF32Header
  /-- The Program Header Entries -/
  interpreted_segments  : List (ELF32ProgramHeaderTableEntry × InterpretedSegment)
  /-- The Sections of the file -/
  interpreted_sections  : List (ELF32SectionHeaderTableEntry × InterpretedSection)
  /-- Bits and Bobs: binary contents not covered by any section or segment -/
  bits_and_bobs         : List (Nat × ByteArray)
  deriving Repr

def mkELF32File? (bytes : ByteArray) : Except String ELF32File := do
  let file_header ← mkELF32Header? bytes

  let program_header_table ← file_header.mkELF32ProgramHeaderTable? bytes

  let section_header_table ← file_header.mkELF32SectionHeaderTable? bytes

  let interpreted_segments ← getInterpretedSegments program_header_table bytes

  let interpreted_sections ← SectionHeaderTableEntry.getInterpretedSections section_header_table file_header bytes

  let bits_and_bobs := getBitsAndBobs file_header section_header_table program_header_table bytes

  .ok {
    file_header
    interpreted_segments
    interpreted_sections
    bits_and_bobs
  }

structure ELF64File where
  /-- The ELF Header -/
  file_header           : ELF64Header
  /-- The Segments of the file -/
  interpreted_segments  : List (ELF64ProgramHeaderTableEntry × InterpretedSegment)
  /-- The Sections of the file -/
  interpreted_sections  : List (ELF64SectionHeaderTableEntry × InterpretedSection)
  /-- Bits and Bobs: binary contents not covered by any section or segment -/
  bits_and_bobs         : List (Nat × ByteArray)
  deriving Repr

def mkELF64File? (bytes : ByteArray) : Except String ELF64File := do
  let file_header ← mkELF64Header? bytes

  let program_header_table ← file_header.mkELF64ProgramHeaderTable? bytes

  let section_header_table ← file_header.mkELF64SectionHeaderTable? bytes

  let interpreted_segments ← getInterpretedSegments program_header_table bytes

  let interpreted_sections ← SectionHeaderTableEntry.getInterpretedSections section_header_table file_header bytes

  let bits_and_bobs := getBitsAndBobs file_header section_header_table program_header_table bytes

  .ok {
    file_header
    interpreted_segments
    interpreted_sections
    bits_and_bobs
  }

inductive RawELFFile where
  | elf32 : ELF32File → RawELFFile
  | elf64 : ELF64File → RawELFFile

def mkRawELFFile? (bytes : ByteArray) : Except String RawELFFile :=
  if h : bytes.size < 5 then throw "Can't determine if this is a 32 or 64 bit binary (not enough bytes)."
  else match bytes.get ⟨0x4, by omega⟩ with
  | 1 => .elf32 <$> mkELF32File? bytes
  | 2 => .elf64 <$> mkELF64File? bytes
  | _ => throw "Can't determine if this is a 32 of 64 bit binary (byte 0x5 of the elf header is bad)"

def RawELFFile.getRawSectionHeaderTableEntries : RawELFFile → List (RawSectionHeaderTableEntry × InterpretedSection)
  | .elf32 elffile => elffile.interpreted_sections.map (λshte => (.elf32 shte.fst, shte.snd))
  | .elf64 elffile => elffile.interpreted_sections.map (λshte => (.elf64 shte.fst, shte.snd))

def RawELFFile.getRawProgramHeaderTableEntries : RawELFFile → List (RawProgramHeaderTableEntry × InterpretedSegment)
  | .elf32 elffile => elffile.interpreted_segments.map (λshte => (.elf32 shte.fst, shte.snd))
  | .elf64 elffile => elffile.interpreted_segments.map (λshte => (.elf64 shte.fst, shte.snd))

def RawELFFile.getRawELFHeader : RawELFFile → RawELFHeader
  | .elf32 elffile => .elf32 elffile.file_header
  | .elf64 elffile => .elf64 elffile.file_header

/--
Get the section of type SHT_SYMTAB.
There's at most one: https://refspecs.linuxbase.org/elf/gabi4+/ch4.sheader.html
-/
def RawELFFile.getSymbolTable? (elffile : RawELFFile)
    : Except String (RawSectionHeaderTableEntry × InterpretedSection) :=
    symbolSections[0]?.elim noSymTable .ok
  where noSymTable := .error "No symbol table present, no st_size given, can't guess byte range"
        symbolSections := elffile.getRawSectionHeaderTableEntries.filter $ λ⟨shte, _⟩↦
          SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_SYMTAB
/--
Get the section of type SHT_DYNSYM
There's at most one: https://refspecs.linuxbase.org/elf/gabi4+/ch4.sheader.html
-/
def RawELFFile.getDynamicSymbolTable? (elffile : RawELFFile)
    : Except String (RawSectionHeaderTableEntry × InterpretedSection) :=
    dynamicSymbolSections[0]?.elim noSymTable .ok
  where noSymTable := .error "No symbol table present, no st_size given, can't guess byte range"
        dynamicSymbolSections := elffile.getRawSectionHeaderTableEntries.filter $ λ⟨shte, _⟩↦
          SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_DYNSYM

instance : ELFHeader RawELFFile where
  e_ident ef      := ELFHeader.e_ident ef.getRawELFHeader
  e_type ef       := ELFHeader.e_type ef.getRawELFHeader
  e_machine ef    := ELFHeader.e_machine ef.getRawELFHeader
  e_version ef    := ELFHeader.e_version ef.getRawELFHeader
  e_entry ef      := ELFHeader.e_entry ef.getRawELFHeader
  e_phoff ef      := ELFHeader.e_phoff ef.getRawELFHeader
  e_shoff ef      := ELFHeader.e_shoff ef.getRawELFHeader
  e_flags ef      := ELFHeader.e_flags ef.getRawELFHeader
  e_ehsize ef     := ELFHeader.e_ehsize ef.getRawELFHeader
  e_phentsize ef  := ELFHeader.e_phentsize ef.getRawELFHeader
  e_phnum ef      := ELFHeader.e_phnum ef.getRawELFHeader
  e_shentsize ef  := ELFHeader.e_shentsize ef.getRawELFHeader
  e_shnum ef      := ELFHeader.e_shnum ef.getRawELFHeader
  e_shstrndx ef   := ELFHeader.e_shstrndx ef.getRawELFHeader

private def getProgramHeaderType (n: Nat) := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 1           => "PT_LOAD"          -- (0x1)
  | 2           => "PT_DYNAMIC"       -- (0x2)
  | 3           => "PT_INTERP"        -- (0x3)
  | 4           => "PT_NOTE"          -- (0x4)
  | 6           => "PT_PHDR"          -- (0x6)
  | 1685382480  => "PT_GNU_EH_FRAME"  -- (0x6474E550)
  | 1685382481  => "PT_GNU_STACK"     -- (0x6474E551)
  | 1685382482  => "PT_GNU_RELRO"     -- (0x6474E552)
  | 1685382483  => "PT_GNU_PROPERTY"  -- (0x6474E553)
  | _ => panic s!"Unrecognized program header type {n}"

private def getProgramHeaderFlag (flagIndex: Nat) := match flagIndex with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0  => "PF_X"  -- (0x1)
  | 1  => "PF_W"  -- (0x2)
  | 2  => "PF_R"  -- (0x4)
  | _ => panic s!"Unrecognized program header flag {flagIndex}"

private def programHeaderFlagsToString (flags: Nat) (indent: String) : String :=
  getFlagBits flags 32
    |> .map (λ flag => s!"{indent}{getProgramHeaderFlag flag} (0x{toHex (1 <<< flag)})\n")
    |> (λ a => .insertionSort a (. < .)) -- sort by flag name
    |> String.join

def RawELFFile.programHeadersToString (elffile : RawELFFile) := Id.run do
  let mut out := "ProgramHeaders [\n"
  let headers := elffile.getRawProgramHeaderTableEntries
  let mut idx := 0
  for ⟨phte, _⟩ in headers do
    let nextHeader :=
        "  ProgramHeader {\n" ++
      s!"    Type: {getProgramHeaderType $ ProgramHeaderTableEntry.p_type phte} (0x{toHex $ ProgramHeaderTableEntry.p_type phte})\n" ++
      s!"    Offset: 0x{toHex $ ProgramHeaderTableEntry.p_offset phte}\n" ++
      s!"    VirtualAddress: 0x{toHex $ ProgramHeaderTableEntry.p_vaddr phte}\n" ++
      s!"    PhysicalAddress: 0x{toHex $ ProgramHeaderTableEntry.p_paddr phte}\n" ++
      s!"    FileSize: {ProgramHeaderTableEntry.p_filesz phte}\n" ++
      s!"    MemSize: {ProgramHeaderTableEntry.p_memsz phte}\n" ++
      s!"    Flags [ (0x{toHex $ ProgramHeaderTableEntry.p_flags phte})\n" ++
      s!"{     programHeaderFlagsToString (ProgramHeaderTableEntry.p_flags phte) "      "}" ++
        "    ]\n" ++
      s!"    Alignment: {ProgramHeaderTableEntry.p_align phte}\n" ++
        "  }\n"
    out := out ++ nextHeader
    idx := idx + 1

  out := out ++ "]"
  return out

private def getSectionHeaderType (n: Nat) := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0           => "SHT_NULL"         -- (0x0)
  | 1           => "SHT_PROGBITS"     -- (0x1)
  | 3           => "SHT_STRTAB"       -- (0x3)
  | 4           => "SHT_RELA"         -- (0x4)
  | 6           => "SHT_DYNAMIC"      -- (0x6)
  | 7           => "SHT_NOTE"         -- (0x7)
  | 8           => "SHT_NOBITS"       -- (0x8)
  | 11          => "SHT_DYNSYM"       -- (0xB)
  | 14          => "SHT_INIT_ARRAY"   -- (0xE)
  | 15          => "SHT_FINI_ARRAY"   -- (0xF)
  | 1879048182  => "SHT_GNU_HASH"     -- (0x6FFFFFF6)
  | 1879048190  => "SHT_GNU_verneed"  -- (0x6FFFFFFE)
  | 1879048191  => "SHT_GNU_versym"   -- (0x6FFFFFFF)
  | _ => panic s!"Unrecognized section header type {n}"

private def getSectionHeaderFlag (flagIndex: Nat) := match flagIndex with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0  => "SHF_WRITE"      -- (0x1)
  | 1  => "SHF_ALLOC"      -- (0x2)
  | 2  => "SHF_EXECINSTR"  -- (0x4)
  | 4  => "SHF_MERGE"      -- (0x10)
  | 5  => "SHF_STRINGS"    -- (0x20)
  | _ => panic s!"Unrecognized section header flag {flagIndex}"

private def sectionHeaderFlagsToString (flags: Nat) (indent: String) : String :=
  getFlagBits flags 64
    |> .map (λ flag => s!"{indent}{getSectionHeaderFlag flag} (0x{toHex (1 <<< flag)})\n")
    |> (λ a => .insertionSort a (. < .)) -- sort by flag name
    |> String.join

def RawELFFile.sectionHeadersToString (elffile : RawELFFile) := Id.run do
  let mut out := "Sections [\n"
  let headers := elffile.getRawSectionHeaderTableEntries
  let mut idx := 0
  for ⟨phte, sec⟩ in headers do
    let name := match sec.section_name_as_string with | .some s => s | _ => ""
    let nextHeader :=
        "  Section {\n" ++
      s!"    Index: {idx}\n" ++
      s!"    Name: {name} ({SectionHeaderTableEntry.sh_name phte})\n" ++
      s!"    Type: {getSectionHeaderType $ SectionHeaderTableEntry.sh_type phte} (0x{toHex $ SectionHeaderTableEntry.sh_type phte})\n" ++
      s!"    Flags [ (0x{toHex $ SectionHeaderTableEntry.sh_flags phte})\n" ++
      s!"{     sectionHeaderFlagsToString (SectionHeaderTableEntry.sh_flags phte) "      "}" ++
        "    ]\n" ++
      s!"    Address: 0x{toHex $ SectionHeaderTableEntry.sh_addr phte}\n" ++
      s!"    Offset: 0x{toHex $ SectionHeaderTableEntry.sh_offset phte}\n" ++
      s!"    Size: {SectionHeaderTableEntry.sh_size phte}\n" ++
      s!"    Link: {SectionHeaderTableEntry.sh_link phte}\n" ++
      s!"    Info: {SectionHeaderTableEntry.sh_info phte}\n" ++
      s!"    AddressAlignment: {SectionHeaderTableEntry.sh_addralign phte}\n" ++
      s!"    EntrySize: {SectionHeaderTableEntry.sh_entsize phte}\n" ++
        "  }\n"
    out := out ++ nextHeader
    idx := idx + 1

  out := out ++ "]"
  return out

def RawELFFile.headersToString (elffile : RawELFFile) :=
  toString elffile.getRawELFHeader ++ "\n" ++
  RawELFFile.sectionHeadersToString elffile ++ "\n" ++
  RawELFFile.programHeadersToString elffile
