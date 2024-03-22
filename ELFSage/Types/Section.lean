import ELFSage.Types.SectionHeaderTable

structure ELF32InterpretedSection where
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

def ELF32SectionHeaderTableEntry.toSection?
  (shte : ELF32SectionHeaderTableEntry)
  (bytes : ByteArray)
  (name : Option String)
  : Except String ELF32InterpretedSection :=
  if bytes.size < shte.sh_offset.toNat + shte.sh_size.toNat
  then .error $
    s! "A section specified in the section header table at offset {shte.sh_offset}, " ++
    s! "with size {shte.sh_size}, runs off the end of the binary."
  else .ok {
    section_name    := shte.sh_name.toNat
    section_type    := shte.sh_type.toNat
    section_flags   := shte.sh_flags.toNat
    section_addr    := shte.sh_addr.toNat
    section_offset  := shte.sh_offset.toNat
    section_size    := shte.sh_size.toNat
    section_link    := shte.sh_link.toNat
    section_info    := shte.sh_info.toNat
    section_align   := shte.sh_addralign.toNat
    section_entsize := shte.sh_entsize.toNat
    section_body    := bytes.extract shte.sh_offset.toNat (shte.sh_offset.toNat + shte.sh_size.toNat)
    section_name_as_string := name
  }
  
structure ELF64InterpretedSection where
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

def ELF64SectionHeaderTableEntry.toSection?
  (shte : ELF64SectionHeaderTableEntry)
  (bytes : ByteArray)
  (name : Option String)
  : Except String ELF64InterpretedSection :=
  if bytes.size < shte.sh_offset.toNat + shte.sh_size.toNat
  then .error $
    s! "A section specified in the section header table at offset {shte.sh_offset}, " ++
    s! "with size {shte.sh_size}, runs off the end of the binary."
  else .ok {
    section_name    := shte.sh_name.toNat
    section_type    := shte.sh_type.toNat
    section_flags   := shte.sh_flags.toNat
    section_addr    := shte.sh_addr.toNat
    section_offset  := shte.sh_offset.toNat
    section_size    := shte.sh_size.toNat
    section_link    := shte.sh_link.toNat
    section_info    := shte.sh_info.toNat
    section_align   := shte.sh_addralign.toNat
    section_entsize := shte.sh_entsize.toNat
    section_body    := bytes.extract shte.sh_offset.toNat (shte.sh_offset.toNat + shte.sh_size.toNat)
    section_name_as_string := name
  }
