import ELFSage.Types.SectionHeaderTable
import ELFSage.Constants.SectionHeaderTable

structure InterpretedSection where
  /-- Name of the section -/
  section_name    : Nat
  /-- Type of the section -/
  section_type    : Nat
  /-- Flags associated with the section -/
  section_flags   : Nat
  /-- Base address of the section in memory -/
  section_addr    : Nat
  /-- Offset from beginning of file -/
  section_offset  : Nat
  /-- Section size in bytes -/
  section_size    : Nat
  /-- Section header table index link -/
  section_link    : Nat
  /-- Extra information, depends on section type -/
  section_info    : Nat
  /-- Alignment constraints for section -/
  section_align   : Nat
  /-- Size of each entry in table, if section is one -/
  section_entsize : Nat
  /-- Body of section -/
  section_body    : ByteArray
  /-- Name of the section, as a string, if the section has one -/
  section_name_as_string : Option String
  deriving Repr

instance : SectionHeaderTableEntry InterpretedSection where
  sh_name sh      := sh.section_name
  sh_type sh      := sh.section_type
  sh_flags sh     := sh.section_flags
  sh_addr sh      := sh.section_addr
  sh_offset sh    := sh.section_offset
  sh_size sh      := sh.section_size
  sh_link sh      := sh.section_link
  sh_info sh      := sh.section_info
  sh_addralign sh := sh.section_align
  sh_entsize sh   := sh.section_entsize
  bytes sh _      := sh.section_body

def SectionHeaderTableEntry.toSection?
  [SectionHeaderTableEntry α]
  (shte : α)
  (bytes : ByteArray)
  (name : Option String)
  : Except String InterpretedSection :=
  let hasBits := sh_type shte != ELFSectionHeaderTableEntry.Type.SHT_NOBITS
  if bytes.size < (sh_offset shte) + (sh_size shte) ∧ hasBits
  then .error $
    s! "{match name with | some n => n | _ => "A section"} specified in the " ++
    s! "section header table at offset 0x{Hex.toHex $ sh_offset shte}, with size 0x{Hex.toHex $ sh_size shte} " ++
    s! "runs off the end of the binary."
  else .ok {
    section_name    := sh_name shte
    section_type    := sh_type shte
    section_flags   := sh_flags shte
    section_addr    := sh_addr shte
    section_offset  := sh_offset shte
    section_size    := sh_size shte
    section_link    := sh_link shte
    section_info    := sh_info shte
    section_align   := sh_addralign shte
    section_entsize := sh_entsize shte
    section_body    := if hasBits
      then bytes.extract (sh_offset shte) (sh_offset shte + sh_size shte)
      else ByteArray.empty
    section_name_as_string := name
  }

def SectionHeaderTableEntry.getInterpretedSections
  [SectionHeaderTableEntry α] (sht : List α)
  [ELFHeader β] (eh : β)
  (bytes : ByteArray)
  : Except String (List (α × InterpretedSection)) := do
  let shstrndx := ELFHeader.e_shstrndx eh
  let section_names ← getSectionNames shstrndx sht bytes
  sht.mapM $ λshte ↦
    if SectionHeaderTableEntry.sh_name shte == 0
    then do
      let sec ← SectionHeaderTableEntry.toSection? shte bytes .none
      return (shte, sec)
    else do
      let name := section_names.stringAt $ SectionHeaderTableEntry.sh_name shte
      let sec ← SectionHeaderTableEntry.toSection? shte bytes name
      return (shte, sec)
