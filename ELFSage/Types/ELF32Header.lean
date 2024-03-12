import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF32Header where
  /-- Identification field -/
  ident    : NByteArray 16
  /-- The object file type -/
  type     : elf32_half
  /-- Required machine architecture -/
  machine  : elf32_half
  /-- Object file version -/
  version  : elf32_word
  /-- Virtual address for transfer of control -/
  entry    : elf32_addr
  /-- Program header table offset in bytes -/
  phoff    : elf32_off
  /-- Section header table offset in bytes -/
  shoff    : elf32_off
  /-- Processor-specific flags -/
  flags    : elf32_word
  /-- ELF header size in bytes -/
  ehsize   : elf32_half
  /-- Program header table entry size in bytes -/
  phentsize: elf32_half
  /-- Number of entries in program header table -/
  phnum    : elf32_half
  /-- Section header table entry size in bytes -/
  shentsize: elf32_half
  /-- Number of entries in section header table -/
  shnum    : elf32_half
  /-- Section header table entry for section name string table -/
  shstrndx : elf32_half
  deriving Repr

/-- A simple parser for extracting an ELF34 header, just a test, no validation -/
def mkELF32Header (bs : ByteArray) (h : bs.size ≥ 52) : ELF32Header := { 
  ident     := NByteArray.extract bs 0x10 (by omega),
  type      := getUInt16from 0x10 (by omega),
  machine   := getUInt16from 0x12 (by omega),
  version   := getUInt32from 0x14 (by omega),
  entry     := getUInt32from 0x18 (by omega),
  phoff     := getUInt32from 0x1C (by omega),
  shoff     := getUInt32from 0x20 (by omega),
  flags     := getUInt32from 0x24 (by omega),
  ehsize    := getUInt16from 0x28 (by omega),
  phentsize := getUInt16from 0x2A (by omega),
  phnum     := getUInt16from 0x2C (by omega),
  shentsize := getUInt16from 0x2E (by omega),
  shnum     := getUInt16from 0x30 (by omega),
  shstrndx  := getUInt16from 0x32 (by omega),
} where
  isBigEndian := bs.get ⟨0x5,by omega⟩ == 2
  getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
  getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
