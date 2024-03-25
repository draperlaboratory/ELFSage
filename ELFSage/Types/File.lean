import ELFSage.Types.ELFHeader
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.Section
import ELFSage.Types.Segment

-- NOTE: this structure has some fairly arbitrary features. linksem for
-- example wants two lists, of program headers and of interpreted segments,
-- which are of the same length as an invariant. It seems at least equally
-- reasonable to have a single field `interpreted_segmenets` of type
-- ProgramHeaderEntry × InterpretedSegment.

structure ELF32File where
  /-- The ELF Header -/
  file_header           : ELF32Header
  /-- The Program Header Entries -/
  program_header_table  : List ELF32ProgramHeaderTableEntry
  /-- The Section Header Entries -/
  section_header_table  : List ELF32SectionHeaderTableEntry
  /-- The Segments of the file -/
  interpreted_segments  : List ELF32InterpretedSegment
  /-- The Sections of the file -/
  interpreted_sections  : List ELF32InterpretedSection
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
  interpreted_segments  : List ELF64InterpretedSegment
  /-- The Sections of the file -/
  interpreted_sections  : List ELF64InterpretedSection
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

--pairs are of the form (offset, offset+size), so for example if the As represent the
--in-range bytes:
--
-- AAABBBAAABBB
--
--Then the ranges would be (0,3),(6,9), and the gaps would be (3,6) (9,12)
def gapsIn (ranges : List (Nat × Nat)) (max : Nat) : List (Nat × Nat) :=
  let sorted := (ranges.toArray.qsort (λr1 r2 ↦ r1.fst < r2.fst)).toList
  toGaps sorted
  where toGaps : List (Nat × Nat) → List (Nat × Nat)
  | [] => []
  | [h] => if h.snd < max then [(h.snd , max)] else []
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

def mkELF64File (bytes : ByteArray) : Except String ELF64File := do
  let file_header ← mkELF64Header? bytes
  let is_bigendian := bytes[0x5]? == .some 2
  let phIndicies := List.range file_header.phnum.toNat
  let shIndicies := List.range file_header.shnum.toNat

  let program_header_table ← phIndicies.mapM $ λidx ↦ do
    let offset := file_header.phoff.toNat + (idx * file_header.phentsize.toNat)
    mkELF64ProgramHeaderTableEntry? is_bigendian bytes offset

  let section_header_table ← shIndicies.mapM $ λidx ↦ do
    let offset := file_header.shoff.toNat + (idx * file_header.shentsize.toNat)
    mkELF64SectionHeaderTableEntry? is_bigendian bytes offset

  let section_names ← if h : file_header.shstrndx.toNat ≥ section_header_table.length
    then .error "the shstrndx from the elf header requests a non-existent section"
    else 
      let shstr_start := section_header_table[file_header.shstrndx.toNat].sh_offset.toNat
      let shstr_end := shstr_start + section_header_table[file_header.shstrndx.toNat].sh_size.toNat
      if shstr_end > bytes.size 
      then .error "the section header string table runs off the end of the binary"
      else .ok $ bytes.extract shstr_start shstr_end

  let interpreted_segments ← program_header_table.mapM 
    $ λphte ↦ phte.toSegment? bytes
  
  let interpreted_sections ← section_header_table.mapM $ λshte ↦ 
    if shte.sh_name == 0 then shte.toSection? bytes .none
    else do
      let name ← section_names.stringAt shte.sh_name.toNat
      shte.toSection? bytes name

  let ranges := (0, file_header.ehsize.toNat) --ELF header
             :: (file_header.phoff.toNat, file_header.phoff.toNat + (file_header.phnum.toNat * file_header.phentsize.toNat)) --Program header entries
             :: (file_header.shoff.toNat, file_header.shoff.toNat + (file_header.shnum.toNat * file_header.shentsize.toNat)) --Section header entries
             :: interpreted_segments.map (λis ↦ (is.segment_offset, is.segment_offset + is.segment_size))  --segments
             ++ interpreted_sections.map (λis ↦ (is.section_offset , is.section_offset + is.section_size)) --sections

  let gaps := gapsIn ranges bytes.size

  let bits_and_bobs := gaps.map λgap ↦ (gap.fst, bytes.extract gap.fst gap.snd)
  
  .ok {
    file_header           
    program_header_table 
    section_header_table
    interpreted_segments
    interpreted_sections
    bits_and_bobs
  }
