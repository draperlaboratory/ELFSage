import ELFSage.Types.ELFHeader
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.StringTable
import ELFSage.Types.Section
import ELFSage.Types.Segment

-- NOTE: this structure has some fairly arbitrary features. linksem for
-- example wants two lists, of program headers and of interpreted segments,
-- which are of the same length as an invariant. It seems at least equally
-- reasonable to have a single field `interpreted_segments` of type
-- ProgramHeaderEntry × InterpretedSegment.

structure ELF32File where
  /-- The ELF Header -/
  file_header           : ELF32Header
  /-- The Program Header Entries -/
  program_header_table  : List ELF32ProgramHeaderTableEntry
  /-- The Section Header Entries -/
  section_header_table  : List ELF32SectionHeaderTableEntry
  /-- The Segments of the file -/
  interpreted_segments  : List InterpretedSegment
  /-- The Sections of the file -/
  interpreted_sections  : List InterpretedSection
  /-- Bits and Bobs: binary contents not covered by any section or segment -/
  bits_and_bobs         : List (Nat × ByteArray)
  deriving Repr

structure ELF64File where
  /-- The ELF Header -/
  file_header           : ELF64Header
  /-- The Program Header Entries -/
  program_header_table  : List ELF64ProgramHeaderTableEntry
  /-- The Section Header Entries -/
  section_header_table  : List ELF64SectionHeaderTableEntry
  /-- The Segments of the file -/
  interpreted_segments  : List InterpretedSegment
  /-- The Sections of the file -/
  interpreted_sections  : List InterpretedSection
  /-- Bits and Bobs: binary contents not covered by any section or segment -/
  bits_and_bobs         : List (Nat × ByteArray)
  deriving Repr

def ByteArray.stringAt (bytes : ByteArray) (idx : Nat) : Except String String :=
  match bytes.findIdx? (· == 0) idx with
  | .none => .error "The provided string table is not null terminated!"
  | .some idx₂ =>
    let range := bytes.extract idx idx₂
    let chars := range.toList.map (λbyte => Char.ofNat byte.toNat)
    .ok $ String.mk chars

def getSectionNames 
  (shstrndx : Nat) 
  [SectionHeaderTableEntry α] (sht : List α) 
  (bytes : ByteArray)
  : Except String ELFStringTable := if h : shstrndx ≥ sht.length
    then .error "the shstrndx from the elf header requests a non-existent section"
    else 
      let shstr_start := SectionHeaderTableEntry.sh_offset sht[shstrndx]
      let shstr_end := shstr_start + SectionHeaderTableEntry.sh_size sht[shstrndx]
      if shstr_end > bytes.size 
      then .error "the section header string table runs off the end of the binary"
      else .ok ⟨bytes.extract shstr_start shstr_end⟩

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

def getInterpretedSections
  [SectionHeaderTableEntry α] (sht : List α) 
  [ELFHeader β] (eh : β)
  (bytes : ByteArray)
  : Except String (List InterpretedSection) := do
  let shstrndx := ELFHeader.e_shstrndx eh
  let section_names ← getSectionNames shstrndx sht bytes
  sht.mapM $ λshte ↦ 
    if SectionHeaderTableEntry.sh_name shte == 0 
    then SectionHeaderTableEntry.toSection? shte bytes .none
    else do
      let name := section_names.stringAt $ SectionHeaderTableEntry.sh_name shte
      SectionHeaderTableEntry.toSection? shte bytes name

def getInterpretedSegments
  [ProgramHeaderTableEntry α] (pht : List α)
  (bytes : ByteArray)
  : Except String (List InterpretedSegment) :=
  pht.mapM $ λphte ↦ ProgramHeaderTableEntry.toSegment? phte bytes

def mkELF64File (bytes : ByteArray) : Except String ELF64File := do
  let file_header ← mkELF64Header? bytes

  let program_header_table ← file_header.mkELF64ProgramHeaderTable? bytes

  let section_header_table ← file_header.mkELF64SectionHeaderTable? bytes

  let interpreted_segments ← getInterpretedSegments program_header_table bytes
  
  let interpreted_sections ← getInterpretedSections section_header_table file_header bytes

  let bits_and_bobs := getBitsAndBobs file_header section_header_table program_header_table bytes
  
  .ok {
    file_header           
    program_header_table 
    section_header_table
    interpreted_segments
    interpreted_sections
    bits_and_bobs
  }

def mkELF32File (bytes : ByteArray) : Except String ELF32File := do
  let file_header ← mkELF32Header? bytes

  let program_header_table ← file_header.mkELF32ProgramHeaderTable? bytes

  let section_header_table ← file_header.mkELF32SectionHeaderTable? bytes

  let interpreted_segments ← getInterpretedSegments program_header_table bytes
  
  let interpreted_sections ← getInterpretedSections section_header_table file_header bytes

  let bits_and_bobs := getBitsAndBobs file_header section_header_table program_header_table bytes
  
  .ok {
    file_header           
    program_header_table 
    section_header_table
    interpreted_segments
    interpreted_sections
    bits_and_bobs
  }
