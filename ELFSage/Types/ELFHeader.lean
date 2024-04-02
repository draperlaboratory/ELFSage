import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray
import ELFSage.Util.Hex

open Hex

class ELFHeader (α : Type) where
  e_ident    : α → NByteArray 16
  /-- The object file type -/
  e_type     : α → Nat
  /-- Required machine architecture -/
  e_machine  : α → Nat
  /-- Object file version -/
  e_version  : α → Nat
  /-- Virtual address for transfer of control -/
  e_entry    : α → Nat
  /-- Program header table offset in bytes -/
  e_phoff    : α → Nat
  /-- Section header table offset in bytes -/
  e_shoff    : α → Nat
  /-- Processor-specific flags -/
  e_flags    : α → Nat
  /-- ELF header size in bytes -/
  e_ehsize   : α → Nat
  /-- Program header table entry size in bytes -/
  e_phentsize: α → Nat
  /-- Number of entries in program header table -/
  e_phnum    : α → Nat
  /-- Section header table entry size in bytes -/
  e_shentsize: α → Nat
  /-- Number of entries in section header table -/
  e_shnum    : α → Nat
  /-- Section header table entry for section name string table -/
  e_shstrndx : α → Nat

def ELFHeader.isBigendian [ELFHeader α] (eh : α) := let ⟨bytes, _⟩ := e_ident eh; bytes[0x5] == 2

def ELFHeader.is64Bit [ELFHeader α] (eh : α) := let ⟨bytes, _⟩ := e_ident eh; bytes[0x4] == 2

def ELFHeader.getSectionHeaderOffsets [ELFHeader α] (eh : α) : List Nat :=
  (List.range (ELFHeader.e_shnum eh)).map λidx ↦ ELFHeader.e_shoff eh + ELFHeader.e_shentsize eh * idx

def ELFHeader.getProgramHeaderOffsets [ELFHeader α] (eh : α) : List Nat :=
  (List.range (ELFHeader.e_phnum eh)).map λidx ↦ ELFHeader.e_phoff eh + ELFHeader.e_phentsize eh * idx

structure ELF64Header where
  /-- Identification field -/
  e_ident    : NByteArray 16
  /-- The object file type -/
  e_type     : elf64_half
  /-- Required machine architecture -/
  e_machine  : elf64_half
  /-- Object file version -/
  e_version  : elf64_word
  /-- Virtual address for transfer of control -/
  e_entry    : elf64_addr
  /-- Program header table offset in bytes -/
  e_phoff    : elf64_off
  /-- Section header table offset in bytes -/
  e_shoff    : elf64_off
  /-- Processor-specific flags -/
  e_flags    : elf64_word
  /-- ELF header size in bytes -/
  e_ehsize   : elf64_half
  /-- Program header table entry size in bytes -/
  e_phentsize: elf64_half
  /-- Number of entries in program header table -/
  e_phnum    : elf64_half
  /-- Section header table entry size in bytes -/
  e_shentsize: elf64_half
  /-- Number of entries in section header table -/
  e_shnum    : elf64_half
  /-- Section header table entry for section name string table -/
  e_shstrndx : elf64_half
  deriving Repr

instance : ELFHeader ELF64Header where
  e_ident eh      := eh.e_ident
  e_type eh       := eh.e_type.toNat
  e_machine eh    := eh.e_machine.toNat
  e_version eh    := eh.e_version.toNat
  e_entry eh      := eh.e_entry.toNat
  e_phoff eh      := eh.e_phoff.toNat
  e_shoff eh      := eh.e_shoff.toNat
  e_flags eh      := eh.e_flags.toNat
  e_ehsize eh     := eh.e_ehsize.toNat
  e_phentsize eh  := eh.e_phentsize.toNat
  e_phnum eh      := eh.e_phnum.toNat
  e_shentsize eh  := eh.e_shentsize.toNat
  e_shnum eh      := eh.e_shnum.toNat
  e_shstrndx eh   := eh.e_shstrndx.toNat

private def getHeaderClass (n: UInt8) : String := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 2 => "64-bit"
  | _ => panic s!"Unrecognized header class {n}"

private def getHeaderDataEncoding (n: UInt8) : String := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 1 => "LittleEndian"
  | _ => panic s!"Unrecognized header data encoding {n}"

private def getHeaderOS (n: UInt8) : String := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0 => "SystemV"
  | _ => panic s!"Unrecognized header OS/ABI {n}"

private def getHeaderType (n: UInt16) : String := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 3 => "SharedObject"
  | _ => panic s!"Unrecognized header type {n}"

private def getHeaderMachine (n: UInt16) : String := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 62 => "EM_X86_64"
  | _ => panic s!"Unrecognized header machine {n}"

instance : ToString ELF64Header where toString eh :=
  let ident (i : Fin 16) := eh.e_ident.bytes.get ⟨ i, by simp [eh.e_ident.sized] ⟩
  let identAsHex (i: Fin 16) := toHex (ident i).toNat
  let identAsHexLength2 (i: Fin 16) := toHexMinLength (ident i).toNat 2

  let val :=
    "ElfHeader {\n" ++
    "  Ident {\n" ++
  s!"    Magic: ({identAsHexLength2 0} {identAsHexLength2 1} {identAsHexLength2 2} {identAsHexLength2 3})\n" ++
  s!"    Class: {getHeaderClass $ ident 4} (0x{identAsHex 4})\n" ++
  s!"    DataEncoding: {getHeaderDataEncoding $ ident 5} (0x{identAsHex 5})\n" ++
  s!"    FileVersion: {ident 6}\n" ++
  s!"    OS/ABI: {getHeaderOS $ ident 7} (0x{identAsHex 7})\n" ++
  s!"    ABIVersion: {ident 8}\n" ++
  s!"    Unused: ({identAsHexLength2 9} {identAsHexLength2 10} {identAsHexLength2 11} {identAsHexLength2 12} {identAsHexLength2 13} {identAsHexLength2 14} {identAsHexLength2 15})\n" ++
    "  }\n" ++
  s!"  Type: {getHeaderType eh.e_type} (0x{toHex eh.e_type.toNat})\n" ++
  s!"  Machine: {getHeaderMachine eh.e_machine} (0x{toHex eh.e_machine.toNat})\n" ++
  s!"  Version: {eh.e_version}\n" ++
  s!"  Entry: 0x{toHex eh.e_entry.toNat}\n" ++
  s!"  ProgramHeaderOffset: 0x{toHex eh.e_phoff.toNat}\n" ++
  s!"  SectionHeaderOffset: 0x{toHex eh.e_shoff.toNat}\n" ++
  s!"  Flags [ (0x{toHex eh.e_flags.toNat})\n" ++
    "  ]\n" ++
  s!"  HeaderSize: {eh.e_ehsize}\n" ++
  s!"  ProgramHeaderEntrySize: {eh.e_phentsize}\n" ++
  s!"  ProgramHeaderCount: {eh.e_phnum}\n" ++
  s!"  SectionHeaderEntrySize: {eh.e_shentsize}\n" ++
  s!"  SectionHeaderCount: {eh.e_shnum}\n" ++
  s!"  StringTableSectionIndex: {eh.e_shstrndx}\n" ++
    "}"
  val

/-- A simple parser for extracting an ELF64 header, just a test, no validation -/
def mkELF64Header (bs : ByteArray) (h : bs.size ≥ 0x40) : ELF64Header := {
  e_ident     := NByteArray.extract bs 0x10 (by omega),
  e_type      := getUInt16from 0x10 (by omega),
  e_machine   := getUInt16from 0x12 (by omega),
  e_version   := getUInt32from 0x14 (by omega),
  e_entry     := getUInt64from 0x18 (by omega),
  e_phoff     := getUInt64from 0x20 (by omega),
  e_shoff     := getUInt64from 0x28 (by omega),
  e_flags     := getUInt32from 0x30 (by omega),
  e_ehsize    := getUInt16from 0x34 (by omega),
  e_phentsize := getUInt16from 0x36 (by omega),
  e_phnum     := getUInt16from 0x38 (by omega),
  e_shentsize := getUInt16from 0x3A (by omega),
  e_shnum     := getUInt16from 0x3C (by omega),
  e_shstrndx  := getUInt16from 0x3E (by omega),
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
  e_ident    : NByteArray 16
  /-- The object file type -/
  e_type     : elf32_half
  /-- Required machine architecture -/
  e_machine  : elf32_half
  /-- Object file version -/
  e_version  : elf32_word
  /-- Virtual address for transfer of control -/
  e_entry    : elf32_addr
  /-- Program header table offset in bytes -/
  e_phoff    : elf32_off
  /-- Section header table offset in bytes -/
  e_shoff    : elf32_off
  /-- Processor-specific flags -/
  e_flags    : elf32_word
  /-- ELF header size in bytes -/
  e_ehsize   : elf32_half
  /-- Program header table entry size in bytes -/
  e_phentsize: elf32_half
  /-- Number of entries in program header table -/
  e_phnum    : elf32_half
  /-- Section header table entry size in bytes -/
  e_shentsize: elf32_half
  /-- Number of entries in section header table -/
  e_shnum    : elf32_half
  /-- Section header table entry for section name string table -/
  e_shstrndx : elf32_half
  deriving Repr

instance : ELFHeader ELF32Header where
  e_ident eh      := eh.e_ident
  e_type eh       := eh.e_type.toNat
  e_machine eh    := eh.e_machine.toNat
  e_version eh    := eh.e_version.toNat
  e_entry eh      := eh.e_entry.toNat
  e_phoff eh      := eh.e_phoff.toNat
  e_shoff eh      := eh.e_shoff.toNat
  e_flags eh      := eh.e_flags.toNat
  e_ehsize eh     := eh.e_ehsize.toNat
  e_phentsize eh  := eh.e_phentsize.toNat
  e_phnum eh      := eh.e_phnum.toNat
  e_shentsize eh  := eh.e_shentsize.toNat
  e_shnum eh      := eh.e_shnum.toNat
  e_shstrndx eh   := eh.e_shstrndx.toNat

instance : ToString ELF32Header where toString eh := toString $ repr eh

/-- A simple parser for extracting an ELF32 header, just a test, no validation -/
def mkELF32Header (bs : ByteArray) (h : bs.size ≥ 0x34) : ELF32Header := {
  e_ident     := NByteArray.extract bs 0x10 (by omega),
  e_type      := getUInt16from 0x10 (by omega),
  e_machine   := getUInt16from 0x12 (by omega),
  e_version   := getUInt32from 0x14 (by omega),
  e_entry     := getUInt32from 0x18 (by omega),
  e_phoff     := getUInt32from 0x1C (by omega),
  e_shoff     := getUInt32from 0x20 (by omega),
  e_flags     := getUInt32from 0x24 (by omega),
  e_ehsize    := getUInt16from 0x28 (by omega),
  e_phentsize := getUInt16from 0x2A (by omega),
  e_phnum     := getUInt16from 0x2C (by omega),
  e_shentsize := getUInt16from 0x2E (by omega),
  e_shnum     := getUInt16from 0x30 (by omega),
  e_shstrndx  := getUInt16from 0x32 (by omega),
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
  e_ident eh      := match eh with | .elf64 eh => eh.e_ident           | .elf32 eh => eh.e_ident
  e_type eh       := match eh with | .elf64 eh => eh.e_type.toNat      | .elf32 eh => eh.e_type.toNat
  e_machine eh    := match eh with | .elf64 eh => eh.e_machine.toNat   | .elf32 eh => eh.e_machine.toNat
  e_version eh    := match eh with | .elf64 eh => eh.e_version.toNat   | .elf32 eh => eh.e_version.toNat
  e_entry eh      := match eh with | .elf64 eh => eh.e_entry.toNat     | .elf32 eh => eh.e_entry.toNat
  e_phoff eh      := match eh with | .elf64 eh => eh.e_phoff.toNat     | .elf32 eh => eh.e_phoff.toNat
  e_shoff eh      := match eh with | .elf64 eh => eh.e_shoff.toNat     | .elf32 eh => eh.e_shoff.toNat
  e_flags eh      := match eh with | .elf64 eh => eh.e_flags.toNat     | .elf32 eh => eh.e_flags.toNat
  e_ehsize eh     := match eh with | .elf64 eh => eh.e_ehsize.toNat    | .elf32 eh => eh.e_ehsize.toNat
  e_phentsize eh  := match eh with | .elf64 eh => eh.e_phentsize.toNat | .elf32 eh => eh.e_phentsize.toNat
  e_phnum eh      := match eh with | .elf64 eh => eh.e_phnum.toNat     | .elf32 eh => eh.e_phnum.toNat
  e_shentsize eh  := match eh with | .elf64 eh => eh.e_shentsize.toNat | .elf32 eh => eh.e_shentsize.toNat
  e_shnum eh      := match eh with | .elf64 eh => eh.e_shnum.toNat     | .elf32 eh => eh.e_shnum.toNat
  e_shstrndx eh   := match eh with | .elf64 eh => eh.e_shstrndx.toNat  | .elf32 eh => eh.e_shstrndx.toNat

instance : ToString RawELFHeader where toString eh :=
  match eh with
    | .elf32 eh => toString eh
    | .elf64 eh => toString eh

def mkRawELFHeader? (bs : ByteArray) : Except String RawELFHeader :=
  if h : bs.size < 5 then throw "Can't determine if this is a 32 or 64 bit binary (not enough bytes)."
  else match bs.get ⟨0x4, by omega⟩ with
  | 1 => .elf32 <$> mkELF32Header? bs
  | 2 => .elf64 <$> mkELF64Header? bs
  | _ => throw "Can't determine if this is a 32 of 64 bit binary (byte 0x5 of the elf header is bad)"
