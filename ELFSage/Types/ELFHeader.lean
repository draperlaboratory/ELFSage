import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

-- TODO: should the ID field be a separate structure?
-- TODO: is the elf64_ prefix kind of redundant?
structure ELF64Header where
  /-- Identification field -/
  elf64_ident    : List UInt8
  /-- The object file type -/
  elf64_type     : elf64_half
  /-- Required machine architecture -/
  elf64_machine  : elf64_half
  /-- Object file version -/
  elf64_version  : elf64_word
  /-- Virtual address for transfer of control -/
  elf64_entry    : elf64_addr
  /-- Program header table offset in bytes -/
  elf64_phoff    : elf64_off
  /-- Section header table offset in bytes -/
  elf64_shoff    : elf64_off
  /-- Processor-specific flags -/
  elf64_flags    : elf64_word
  /-- ELF header size in bytes -/
  elf64_ehsize   : elf64_half
  /-- Program header table entry size in bytes -/
  elf64_phentsize: elf64_half
  /-- Number of entries in program header table -/
  elf64_phnum    : elf64_half
  /-- Section header table entry size in bytes -/
  elf64_shentsize: elf64_half
  /-- Number of entries in section header table -/
  elf64_shnum    : elf64_half
  /-- Section header table entry for section name string table -/
  elf64_shstrndx : elf64_half
  deriving Repr

/-- A simple parser for extracting an ELF header, just a test, no validation -/
def mkELF64Header (bs : ByteArray) (h : bs.size ≥ 64) : ELF64Header := { 
  elf64_ident     := (bs.extract 0x0 0x9).toList
  elf64_type      := getUInt16from 0x10 (by omega),
  elf64_machine   := getUInt16from 0x12 (by omega),
  elf64_version   := getUInt32from 0x14 (by omega),
  elf64_entry     := getUInt64from 0x18 (by omega),
  elf64_phoff     := getUInt64from 0x20 (by omega),
  elf64_shoff     := getUInt64from 0x28 (by omega),
  elf64_flags     := getUInt32from 0x30 (by omega),
  elf64_ehsize    := getUInt16from 0x34 (by omega),
  elf64_phentsize := getUInt16from 0x36 (by omega),
  elf64_phnum     := getUInt16from 0x38 (by omega),
  elf64_shentsize := getUInt16from 0x3A (by omega),
  elf64_shnum     := getUInt16from 0x3C (by omega),
  elf64_shstrndx  := getUInt16from 0x3E (by omega),
} where
  isBigEndian := bs.get ⟨0x5,by omega⟩ == 1
  getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
  getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
  getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
