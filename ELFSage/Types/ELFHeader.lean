import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure RawELFHeader where
  ident    : NByteArray 16
  /-- The object file type -/
  type     : Nat
  /-- Required machine architecture -/
  machine  : Nat
  /-- Object file version -/
  version  : Nat
  /-- Virtual address for transfer of control -/
  entry    : Nat
  /-- Program header table offset in bytes -/
  phoff    : Nat
  /-- Section header table offset in bytes -/
  shoff    : Nat
  /-- Processor-specific flags -/
  flags    : Nat
  /-- ELF header size in bytes -/
  ehsize   : Nat
  /-- Program header table entry size in bytes -/
  phentsize: Nat
  /-- Number of entries in program header table -/
  phnum    : Nat
  /-- Section header table entry size in bytes -/
  shentsize: Nat
  /-- Number of entries in section header table -/
  shnum    : Nat
  /-- Section header table entry for section name string table -/
  shstrndx : Nat
  deriving Repr

def RawELFHeader.isBigendian (rh : RawELFHeader) := let ⟨bytes, _⟩ := rh.ident; bytes[0x5] == 2
def RawELFHeader.is64Bit (rh : RawELFHeader) := let ⟨bytes, _⟩ := rh.ident; bytes[0x4] == 2

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

def ELF64Header.toRawELFHeader (eh : ELF64Header) : RawELFHeader := {
  ident     := eh.ident
  type      := eh.type.toNat
  machine   := eh.machine.toNat
  version   := eh.version.toNat
  entry     := eh.entry.toNat
  phoff     := eh.phoff.toNat
  shoff     := eh.shoff.toNat
  flags     := eh.flags.toNat
  ehsize    := eh.ehsize.toNat
  phentsize := eh.phentsize.toNat
  phnum     := eh.phnum.toNat
  shentsize := eh.shentsize.toNat
  shnum     := eh.shnum.toNat
  shstrndx  := eh.shstrndx.toNat
}

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
def mkELF32Header (bs : ByteArray) (h : bs.size ≥ 0x34) : ELF32Header := { 
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

def ELF32Header.toRawELFHeader (eh : ELF32Header) : RawELFHeader := {
  ident     := eh.ident
  type      := eh.type.toNat
  machine   := eh.machine.toNat
  version   := eh.version.toNat
  entry     := eh.entry.toNat
  phoff     := eh.phoff.toNat
  shoff     := eh.shoff.toNat
  flags     := eh.flags.toNat
  ehsize    := eh.ehsize.toNat
  phentsize := eh.phentsize.toNat
  phnum     := eh.phnum.toNat
  shentsize := eh.shentsize.toNat
  shnum     := eh.shnum.toNat
  shstrndx  := eh.shstrndx.toNat
}

def mkRawELFHeader? (bs : ByteArray) : Except String RawELFHeader :=
  if h : bs.size < 6 then throw e1
  else match bs.get ⟨0x5, by omega⟩ with
  | 1 => if h : bs.size ≥ 0x34 then pure (mkELF32Header bs h).toRawELFHeader else throw e2
  | 2 => if h : bs.size ≥ 0x40 then pure (mkELF64Header bs h).toRawELFHeader else throw e3
  | _ => throw e4
  where
    e1 := "Can't determine if this is a 32 or 64 bit binary (not enough bytes)."
    e2 := "Can't parse - ELF header specifies 32 bit, but there aren't enough bytes."
    e3 := "Can't parse - ELF header specifies 64 bit, but there aren't enough bytes."
    e4 := "Can't determine if this is a 32 of 64 bit binary (byte 0x5 of the elf header is bad)"
