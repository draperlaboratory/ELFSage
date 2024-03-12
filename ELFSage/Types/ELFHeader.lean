import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64Header where
  /-- Identification field -/
  ident    : NByteArray 16
  /-- The object file type -/
  type     : elf64_half
  /-- Required machine architecture -/
  machine  : elf64_half
  /-- Object file version -/
  version  : elf64_word
  /-- Virtual address for transfer of control -/
  entry    : elf64_addr
  /-- Program header table offset in bytes -/
  phoff    : elf64_off
  /-- Section header table offset in bytes -/
  shoff    : elf64_off
  /-- Processor-specific flags -/
  flags    : elf64_word
  /-- ELF header size in bytes -/
  ehsize   : elf64_half
  /-- Program header table entry size in bytes -/
  phentsize: elf64_half
  /-- Number of entries in program header table -/
  phnum    : elf64_half
  /-- Section header table entry size in bytes -/
  shentsize: elf64_half
  /-- Number of entries in section header table -/
  shnum    : elf64_half
  /-- Section header table entry for section name string table -/
  shstrndx : elf64_half
  deriving Repr

/-- A simple parser for extracting an ELF64 header, just a test, no validation -/
def mkELF64Header (bs : ByteArray) (h : bs.size ≥ 64) : ELF64Header := { 
  ident     := NByteArray.extract bs 0x10 (by omega),
  type      := getUInt16from 0x10 (by omega),
  machine   := getUInt16from 0x12 (by omega),
  version   := getUInt32from 0x14 (by omega),
  entry     := getUInt64from 0x18 (by omega),
  phoff     := getUInt64from 0x20 (by omega),
  shoff     := getUInt64from 0x28 (by omega),
  flags     := getUInt32from 0x30 (by omega),
  ehsize    := getUInt16from 0x34 (by omega),
  phentsize := getUInt16from 0x36 (by omega),
  phnum     := getUInt16from 0x38 (by omega),
  shentsize := getUInt16from 0x3A (by omega),
  shnum     := getUInt16from 0x3C (by omega),
  shstrndx  := getUInt16from 0x3E (by omega),
} where
  isBigEndian := bs.get ⟨0x5,by omega⟩ == 2
  getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
  getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
  getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

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

/-- Represents the ELF header's ident field structure -/
class ELFIdent (α : Type) where
  /-- is the ELF binary bigendian? -/
  isBigendian : α → Bool
  /-- Does the ELF file use 64 bit addresses? -/
  is64Bit     : α → Bool

instance : ELFIdent ELF64Header where
  isBigendian h := let ⟨bytes, _⟩ := h.ident; bytes[0x5] == 2
  is64Bit h := let ⟨bytes, _⟩ := h.ident; bytes[0x4] == 2 --should never be false

instance : ELFIdent ELF32Header where
  isBigendian h := let ⟨bytes, _⟩ := h.ident; bytes[0x5] == 2
  is64Bit h := let ⟨bytes, _⟩ := h.ident; bytes[0x4] == 2 --should never be false
