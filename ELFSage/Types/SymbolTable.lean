import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

class SymbolTableEntry (α : Type) where
  /-- Index into the object file's string table -/
  st_name  : α → Nat
  /-- Specifies the symbol's type and binding attributes -/
  st_info  : α → Nat
  /-- Currently specifies the symbol's visibility -/
  st_other : α → Nat
  /-- Section header index symbol is defined with respect to -/
  st_shndx : α → Nat
  /-- Gives the value of the associated symbol -/
  st_value : α → Nat
  /-- Size of the associated symbol -/
  st_size  : α → Nat

structure ELF64SymbolTableEntry where
  /-- Index into the object file's string table -/
  st_name  : elf64_word
  /-- Specifies the symbol's type and binding attributes -/
  st_info  : UInt8
  /-- Currently specifies the symbol's visibility -/
  st_other : UInt8
  /-- Section header index symbol is defined with respect to -/
  st_shndx : elf64_half
  /-- Gives the value of the associated symbol -/
  st_value : elf64_addr
  /-- Size of the associated symbol -/
  st_size  : elf64_xword
  deriving Repr

instance : SymbolTableEntry ELF64SymbolTableEntry where
  st_name ste  := ste.st_name.toNat
  st_info ste  := ste.st_info.toNat
  st_other ste := ste.st_other.toNat
  st_shndx ste := ste.st_shndx.toNat
  st_value ste := ste.st_value.toNat
  st_size ste  := ste.st_size.toNat

def mkELF64SymbolTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x18) :
  ELF64SymbolTableEntry := {
    st_name  := getUInt32from (offset + 0x00) (by omega),
    st_info  := bs.get ⟨offset + 0x4, by omega⟩,
    st_other := bs.get ⟨offset + 0x5, by omega⟩,
    st_shndx := getUInt16from (offset + 0x6) (by omega),
    st_value := getUInt64from (offset + 0x8) (by omega),
    st_size  := getUInt64from (offset + 0x10) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

structure ELF32SymbolTableEntry where
  /-- Index into the object file's string table -/
  st_name  : elf32_word
  /-- Gives the value of the associated symbol -/
  st_value : elf32_addr
  /-- Size of the associated symbol -/
  st_size  : elf32_word
  /-- Specifies the symbol's type and binding attributes -/
  st_info  : UInt8
  /-- Currently specifies the symbol's visibility -/
  st_other : UInt8
  /-- Section header index symbol is defined with respect to -/
  st_shndx : elf32_half
  deriving Repr

instance : SymbolTableEntry ELF32SymbolTableEntry where
  st_name ste  := ste.st_name.toNat
  st_info ste  := ste.st_info.toNat
  st_other ste := ste.st_other.toNat
  st_shndx ste := ste.st_shndx.toNat
  st_value ste := ste.st_value.toNat
  st_size ste  := ste.st_size.toNat

def mkELF32SymbolTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0xd) :
  ELF32SymbolTableEntry := {
    st_name  := getUInt32from (offset + 0x00) (by omega),
    st_value := getUInt32from (offset + 0x04) (by omega),
    st_size  := getUInt32from (offset + 0x08) (by omega),
    st_info  := bs.get ⟨offset + 0x9, by omega⟩,
    st_other := bs.get ⟨offset + 0xa, by omega⟩,
    st_shndx := getUInt16from (offset + 0xb) (by omega) ,
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

inductive RawSymbolTableEntry :=
  | elf32 : ELF32SymbolTableEntry → RawSymbolTableEntry
  | elf64 : ELF64SymbolTableEntry → RawSymbolTableEntry
  deriving Repr

instance : SymbolTableEntry RawSymbolTableEntry where
  st_name ste  := match ste with | .elf64 ste => ste.st_name.toNat  | .elf32 ste => ste.st_name.toNat 
  st_info ste  := match ste with | .elf64 ste => ste.st_info.toNat  | .elf32 ste => ste.st_info.toNat 
  st_other ste := match ste with | .elf64 ste => ste.st_other.toNat | .elf32 ste => ste.st_other.toNat
  st_shndx ste := match ste with | .elf64 ste => ste.st_shndx.toNat | .elf32 ste => ste.st_shndx.toNat
  st_value ste := match ste with | .elf64 ste => ste.st_value.toNat | .elf32 ste => ste.st_value.toNat
  st_size ste  := match ste with | .elf64 ste => ste.st_size.toNat  | .elf32 ste => ste.st_size.toNat 

def mkRawSymbolTableEntry?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawSymbolTableEntry := 
  match is64Bit with
  | true   => 
    if h : bs.size - offset ≥ 0x18
    then pure (.elf64 $ mkELF64SymbolTableEntry isBigendian bs offset h)
    else throw $ err 0x18
  | false  => 
    if h : bs.size - offset ≥ 0xd
    then pure (.elf32 $ mkELF32SymbolTableEntry isBigendian bs offset h)
    else throw $ err 0xd
  where
    err size := s! "Symbol table entry offset {offset} doesn't leave enough space for the entry, " ++
                s! "which requires {size} bytes."
