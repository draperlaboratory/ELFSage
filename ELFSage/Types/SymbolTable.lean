import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64SymbolTableEntry where
  elf64_st_name  : elf64_word     -- Index into the object file's string table 
  elf64_st_info  : UInt8          -- Specifies the symbol's type and binding attributes 
  elf64_st_other : UInt8          -- Currently specifies the symbol's visibility 
  elf64_st_shndx : elf64_half     -- Section header index symbol is defined with respect to 
  elf64_st_value : elf64_addr     -- Gives the value of the associated symbol 
  elf64_st_size  : elf64_xword    -- Size of the associated symbol
  deriving Repr

def mkELF64SymbolTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray) 
  (offset : Nat) 
  (h : bs.size - offset ≥ 0x18) :
  ELF64SymbolTableEntry := {
    elf64_st_name  := getUInt32from (offset + 0x00) (by omega),
    elf64_st_info  := bs.get ⟨0x4, by omega⟩,
    elf64_st_other := bs.get ⟨0x5, by omega⟩,
    elf64_st_shndx := getUInt16from 0x6 (by omega) ,
    elf64_st_value := getUInt64from 0x8 (by omega),
    elf64_st_size  := getUInt64from 0x10 (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
