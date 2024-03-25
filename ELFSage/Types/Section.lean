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

def SectionHeaderTableEntry.toSection?
  [SectionHeaderTableEntry α]
  (shte : α)
  (bytes : ByteArray)
  (name : Option String)
  : Except String InterpretedSection :=
  if bytes.size < (sh_offset shte) + (sh_size shte)
  then .error $
    s! "A section specified in the section header table at offset {sh_offset shte}, " ++
    s! "with size {sh_size shte}, runs off the end of the binary."
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
    section_body    := bytes.extract (sh_offset shte) (sh_offset shte + sh_size shte)
    section_name_as_string := name
  }
