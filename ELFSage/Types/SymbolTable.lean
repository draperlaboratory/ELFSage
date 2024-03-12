import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

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

def mkELF64SymbolTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x18) :
  ELF64SymbolTableEntry := {
    st_name  := getUInt32from (offset + 0x00) (by omega),
    st_info  := bs.get ⟨0x4, by omega⟩,
    st_other := bs.get ⟨0x5, by omega⟩,
    st_shndx := getUInt16from 0x6 (by omega) ,
    st_value := getUInt64from 0x8 (by omega),
    st_size  := getUInt64from 0x10 (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
