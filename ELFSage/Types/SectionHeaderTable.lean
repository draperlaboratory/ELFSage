import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64SectionHeaderTableEntry where
  elf64_sh_name      : elf64_word  -- Name of the section
  elf64_sh_type      : elf64_word  -- Type of the section and its semantics
  elf64_sh_flags     : elf64_xword -- Flags associated with the section
  elf64_sh_addr      : elf64_addr  -- Address of first byte of section in memory image
  elf64_sh_offset    : elf64_off   -- Offset from beginning of file of first byte of section
  elf64_sh_size      : elf64_xword -- Section size in bytes
  elf64_sh_link      : elf64_word  -- Section header table index link
  elf64_sh_info      : elf64_word  -- Extra information, contents depends on type of section
  elf64_sh_addralign : elf64_xword -- Alignment constraints for section
  elf64_sh_entsize   : elf64_xword -- Size of each entry in table, if section is one
  deriving Repr
  
def mkELF64SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray) 
  (offset : Nat) 
  (h : bs.size - offset ≥ 0x40) :
  ELF64SectionHeaderTableEntry := {
    elf64_sh_name      := getUInt32from (offset + 0x00) (by omega),
    elf64_sh_type      := getUInt32from (offset + 0x04) (by omega),
    elf64_sh_flags     := getUInt64from (offset + 0x08) (by omega),
    elf64_sh_addr      := getUInt64from (offset + 0x10) (by omega),
    elf64_sh_offset    := getUInt64from (offset + 0x18) (by omega),
    elf64_sh_size      := getUInt64from (offset + 0x20) (by omega),
    elf64_sh_link      := getUInt32from (offset + 0x28) (by omega),
    elf64_sh_info      := getUInt32from (offset + 0x2C) (by omega),
    elf64_sh_addralign := getUInt64from (offset + 0x30) (by omega),
    elf64_sh_entsize   := getUInt64from (offset + 0x38) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
