import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure RawSectionHeaderTableEntry where
  /-- Name of the section -/
  sh_name      : Nat
  /-- Type of the section and its semantics -/
  sh_type      : Nat
  /-- Flags associated with the section -/
  sh_flags     : Nat
  /-- Address of first byte of section in memory image -/
  sh_addr      : Nat
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : Nat
  /-- Section size in bytes -/
  sh_size      : Nat
  /-- Section header table index link -/
  sh_link      : Nat
  /-- Extra information, contents depends on type of section -/
  sh_info      : Nat
  /-- Alignment constraints for section -/
  sh_addralign : Nat
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : Nat
  deriving Repr

structure ELF64SectionHeaderTableEntry where
  /-- Name of the section -/
  sh_name      : elf64_word
  /-- Type of the section and its semantics -/
  sh_type      : elf64_word
  /-- Flags associated with the section -/
  sh_flags     : elf64_xword
  /-- Address of first byte of section in memory image -/
  sh_addr      : elf64_addr
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : elf64_off
  /-- Section size in bytes -/
  sh_size      : elf64_xword
  /-- Section header table index link -/
  sh_link      : elf64_word
  /-- Extra information, contents depends on type of section -/
  sh_info      : elf64_word
  /-- Alignment constraints for section -/
  sh_addralign : elf64_xword
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : elf64_xword
  deriving Repr

def mkELF64SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x40) :
  ELF64SectionHeaderTableEntry := {
    sh_name      := getUInt32from (offset + 0x00) (by omega),
    sh_type      := getUInt32from (offset + 0x04) (by omega),
    sh_flags     := getUInt64from (offset + 0x08) (by omega),
    sh_addr      := getUInt64from (offset + 0x10) (by omega),
    sh_offset    := getUInt64from (offset + 0x18) (by omega),
    sh_size      := getUInt64from (offset + 0x20) (by omega),
    sh_link      := getUInt32from (offset + 0x28) (by omega),
    sh_info      := getUInt32from (offset + 0x2C) (by omega),
    sh_addralign := getUInt64from (offset + 0x30) (by omega),
    sh_entsize   := getUInt64from (offset + 0x38) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

def ELF64SectionHeaderTableEntry.toRawSectionHeaderTableEntry
  (she : ELF64SectionHeaderTableEntry)
  : RawSectionHeaderTableEntry := {
    sh_name      := she.sh_name.toNat
    sh_type      := she.sh_type.toNat
    sh_flags     := she.sh_flags.toNat
    sh_addr      := she.sh_addr.toNat
    sh_offset    := she.sh_offset.toNat
    sh_size      := she.sh_size.toNat
    sh_link      := she.sh_link.toNat
    sh_info      := she.sh_info.toNat
    sh_addralign := she.sh_addralign.toNat
    sh_entsize   := she.sh_entsize.toNat
  }

structure ELF32SectionHeaderTableEntry where
  /-- Name of the section -/
  sh_name      : elf32_word
  /-- Type of the section and its semantics -/
  sh_type      : elf32_word
  /-- Flags associated with the section -/
  sh_flags     : elf32_word
  /-- Address of first byte of section in memory image -/
  sh_addr      : elf32_addr
  /-- Offset from beginning of file of first byte of section -/
  sh_offset    : elf32_off
  /-- Section size in bytes -/
  sh_size      : elf32_word
  /-- Section header table index link -/
  sh_link      : elf32_word
  /-- Extra information, contents depends on type of section -/
  sh_info      : elf32_word
  /-- Alignment constraints for section -/
  sh_addralign : elf32_word
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  sh_entsize   : elf32_word
  deriving Repr


def mkELF32SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x28) :
  ELF32SectionHeaderTableEntry := {
    sh_name      := getUInt32from (offset + 0x00) (by omega),
    sh_type      := getUInt32from (offset + 0x04) (by omega),
    sh_flags     := getUInt32from (offset + 0x08) (by omega),
    sh_addr      := getUInt32from (offset + 0x0C) (by omega),
    sh_offset    := getUInt32from (offset + 0x10) (by omega),
    sh_size      := getUInt32from (offset + 0x14) (by omega),
    sh_link      := getUInt32from (offset + 0x18) (by omega),
    sh_info      := getUInt32from (offset + 0x1C) (by omega),
    sh_addralign := getUInt32from (offset + 0x20) (by omega),
    sh_entsize   := getUInt32from (offset + 0x24) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

def ELF32SectionHeaderTableEntry.toRawSectionHeaderTableEntry
  (she : ELF32SectionHeaderTableEntry)
  : RawSectionHeaderTableEntry := {
    sh_name      := she.sh_name.toNat
    sh_type      := she.sh_type.toNat
    sh_flags     := she.sh_flags.toNat
    sh_addr      := she.sh_addr.toNat
    sh_offset    := she.sh_offset.toNat
    sh_size      := she.sh_size.toNat
    sh_link      := she.sh_link.toNat
    sh_info      := she.sh_info.toNat
    sh_addralign := she.sh_addralign.toNat
    sh_entsize   := she.sh_entsize.toNat
  }

def mkRawSectionHeaderTableEntry?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawSectionHeaderTableEntry := 
  match is64Bit with
  | true   => 
    if h : bs.size - offset ≥ 0x40 
    then pure (mkELF64SectionHeaderTableEntry isBigendian bs offset h).toRawSectionHeaderTableEntry
    else throw $ err 0x40
  | false  => 
    if h : bs.size - offset ≥ 0x28 
    then pure (mkELF32SectionHeaderTableEntry isBigendian bs offset h).toRawSectionHeaderTableEntry
    else throw $ err 0x28
  where
    err size := s! "Section header table entry offset {offset} doesn't leave enough space for the entry, " ++
                s! "which requires {size} bytes."
