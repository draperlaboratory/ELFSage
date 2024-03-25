import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

class ELFHeader (α : Type) where
  ident    : α → NByteArray 16
  /-- The object file type -/
  type     : α → Nat
  /-- Required machine architecture -/
  machine  : α → Nat
  /-- Object file version -/
  version  : α → Nat
  /-- Virtual address for transfer of control -/
  entry    : α → Nat
  /-- Program header table offset in bytes -/
  phoff    : α → Nat
  /-- Section header table offset in bytes -/
  shoff    : α → Nat
  /-- Processor-specific flags -/
  flags    : α → Nat
  /-- ELF header size in bytes -/
  ehsize   : α → Nat
  /-- Program header table entry size in bytes -/
  phentsize: α → Nat
  /-- Number of entries in program header table -/
  phnum    : α → Nat
  /-- Section header table entry size in bytes -/
  shentsize: α → Nat
  /-- Number of entries in section header table -/
  shnum    : α → Nat
  /-- Section header table entry for section name string table -/
  shstrndx : α → Nat

def ELFHeader.isBigendian [ELFHeader α] (eh : α) := let ⟨bytes, _⟩ := ident eh; bytes[0x5] == 2
def ELFHeader.is64Bit [ELFHeader α] (eh : α) := let ⟨bytes, _⟩ := ident eh; bytes[0x4] == 2

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

instance : ELFHeader ELF64Header where
  ident eh      := eh.ident
  type eh       := eh.type.toNat
  machine eh    := eh.machine.toNat
  version eh    := eh.version.toNat
  entry eh      := eh.entry.toNat
  phoff eh      := eh.phoff.toNat
  shoff eh      := eh.shoff.toNat
  flags eh      := eh.flags.toNat
  ehsize eh     := eh.ehsize.toNat
  phentsize eh  := eh.phentsize.toNat
  phnum eh      := eh.phnum.toNat
  shentsize eh  := eh.shentsize.toNat
  shnum eh      := eh.shnum.toNat
  shstrndx eh   := eh.shstrndx.toNat

/-- A simple parser for extracting an ELF64 header, just a test, no validation -/
def mkELF64Header (bs : ByteArray) (h : bs.size ≥ 0x40) : ELF64Header := { 
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

def mkELF64Header? (bs: ByteArray) : Except String ELF64Header :=
  if h : bs.size ≥ 0x40 then .ok $ mkELF64Header bs h
  else .error "We're looking for a 64 bit ELF header but there aren't enough bytes."

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

instance : ELFHeader ELF64Header where
  ident eh      := eh.ident
  type eh       := eh.type.toNat
  machine eh    := eh.machine.toNat
  version eh    := eh.version.toNat
  entry eh      := eh.entry.toNat
  phoff eh      := eh.phoff.toNat
  shoff eh      := eh.shoff.toNat
  flags eh      := eh.flags.toNat
  ehsize eh     := eh.ehsize.toNat
  phentsize eh  := eh.phentsize.toNat
  phnum eh      := eh.phnum.toNat
  shentsize eh  := eh.shentsize.toNat
  shnum eh      := eh.shnum.toNat
  shstrndx eh   := eh.shstrndx.toNat

/-- A simple parser for extracting an ELF32 header, just a test, no validation -/
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

def mkELF32Header? (bs: ByteArray) : Except String ELF32Header :=
  if h : bs.size ≥ 0x34 then .ok $ mkELF32Header bs h
  else .error "We're looking for a 32 bit ELF header but there aren't enough bytes."

inductive RawELFHeader :=
  | elf32 : ELF32Header → RawELFHeader
  | elf64 : ELF64Header → RawELFHeader
  deriving Repr

instance : ELFHeader RawELFHeader where
  ident eh      := match eh with | .elf64 eh => eh.ident           | .elf32 eh => eh.ident
  type eh       := match eh with | .elf64 eh => eh.type.toNat      | .elf32 eh => eh.type.toNat
  machine eh    := match eh with | .elf64 eh => eh.machine.toNat   | .elf32 eh => eh.machine.toNat
  version eh    := match eh with | .elf64 eh => eh.version.toNat   | .elf32 eh => eh.version.toNat
  entry eh      := match eh with | .elf64 eh => eh.entry.toNat     | .elf32 eh => eh.entry.toNat
  phoff eh      := match eh with | .elf64 eh => eh.phoff.toNat     | .elf32 eh => eh.phoff.toNat
  shoff eh      := match eh with | .elf64 eh => eh.shoff.toNat     | .elf32 eh => eh.shoff.toNat
  flags eh      := match eh with | .elf64 eh => eh.flags.toNat     | .elf32 eh => eh.flags.toNat
  ehsize eh     := match eh with | .elf64 eh => eh.ehsize.toNat    | .elf32 eh => eh.ehsize.toNat
  phentsize eh  := match eh with | .elf64 eh => eh.phentsize.toNat | .elf32 eh => eh.phentsize.toNat
  phnum eh      := match eh with | .elf64 eh => eh.phnum.toNat     | .elf32 eh => eh.phnum.toNat
  shentsize eh  := match eh with | .elf64 eh => eh.shentsize.toNat | .elf32 eh => eh.shentsize.toNat
  shnum eh      := match eh with | .elf64 eh => eh.shnum.toNat     | .elf32 eh => eh.shnum.toNat
  shstrndx eh   := match eh with | .elf64 eh => eh.shstrndx.toNat  | .elf32 eh => eh.shstrndx.toNat

def mkRawELFHeader? (bs : ByteArray) : Except String RawELFHeader :=
  if h : bs.size < 5 then throw "Can't determine if this is a 32 or 64 bit binary (not enough bytes)."
  else match bs.get ⟨0x4, by omega⟩ with
  | 1 => .elf32 <$> mkELF32Header? bs
  | 2 => .elf64 <$> mkELF64Header? bs
  | _ => throw "Can't determine if this is a 32 of 64 bit binary (byte 0x5 of the elf header is bad)"
