import ELFSage.Types.ELFHeader
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.Section
import ELFSage.Types.Segment

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

  let interpreted_sections ← getInterpretedSections section_header_table file_header bytes

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

  let interpreted_sections ← getInterpretedSections section_header_table file_header bytes

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
