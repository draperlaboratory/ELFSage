import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64ProgramHeaderTableEntry where
  /-- Type of the segment -/
  p_type   : elf64_word
  /-- Segment flags -/
  p_flags  : elf64_word
  /-- Offset from beginning of file for segment -/
  p_offset : elf64_off
  /-- Virtual address for segment in memory -/
  p_vaddr  : elf64_addr
  /-- Physical address for segment -/
  p_paddr  : elf64_addr
  /-- Size of segment in file, in bytes -/
  p_filesz : elf64_xword
  /-- Size of segment in memory image, in bytes -/
  p_memsz  : elf64_xword
  /-- Segment alignment memory for memory and file -/
  p_align  : elf64_xword
  deriving Repr

def mkELF64ProgramHeaderTableEntry 
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x38) :
  ELF64ProgramHeaderTableEntry := {
    p_type   := getUInt32from (offset + 0x00) (by omega), 
    p_flags  := getUInt32from (offset + 0x04) (by omega),
    p_offset := getUInt64from (offset + 0x08) (by omega),
    p_vaddr  := getUInt64from (offset + 0x10) (by omega),
    p_paddr  := getUInt64from (offset + 0x18) (by omega),
    p_filesz := getUInt64from (offset + 0x20) (by omega),
    p_memsz  := getUInt64from (offset + 0x28) (by omega),
    p_align  := getUInt64from (offset + 0x30) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

structure ELF32ProgramHeaderTableEntry where
  /-- Type of the segment -/
  p_type   : elf32_word
  /-- Offset from beginning of file for segment -/
  p_offset : elf32_off
  /-- Virtual address for segment in memory -/
  p_vaddr  : elf32_addr
  /-- Physical address for segment -/
  p_paddr  : elf32_addr
  /-- Size of segment in file, in bytes -/
  p_filesz : elf64_word
  /-- Size of segment in memory image, in bytes -/
  p_memsz  : elf64_word
  /-- Segment flags -/
  p_flags  : elf32_word
  /-- Segment alignment memory for memory and file -/
  p_align  : elf64_word
  deriving Repr

def mkELF32ProgramHeaderTableEntry 
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x20) :
  ELF32ProgramHeaderTableEntry := {
    p_type   := getUInt32from (offset + 0x00) (by omega), 
    p_offset := getUInt32from (offset + 0x04) (by omega),
    p_vaddr  := getUInt32from (offset + 0x08) (by omega),
    p_paddr  := getUInt32from (offset + 0x0C) (by omega),
    p_filesz := getUInt32from (offset + 0x10) (by omega),
    p_memsz  := getUInt32from (offset + 0x14) (by omega),
    p_flags  := getUInt32from (offset + 0x18) (by omega),
    p_align  := getUInt32from (offset + 0x1C) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
