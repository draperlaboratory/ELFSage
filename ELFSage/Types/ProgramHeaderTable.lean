import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64ProgramHeaderTableEntry where
  elf64_p_type   : elf64_word  -- Type of the segment
  elf64_p_flags  : elf64_word  -- Segment flags
  elf64_p_offset : elf64_off   -- Offset from beginning of file for segment
  elf64_p_vaddr  : elf64_addr  -- Virtual address for segment in memory
  elf64_p_paddr  : elf64_addr  -- Physical address for segment
  elf64_p_filesz : elf64_xword -- Size of segment in file, in bytes
  elf64_p_memsz  : elf64_xword -- Size of segment in memory image, in bytes
  elf64_p_align  : elf64_xword -- Segment alignment memory for memory and file
  deriving Repr

/- XXX: 
  We're building in the assumption that the entries are at least 56 bytes, rather
  than using phentsize. As far as I can tell, this is what REMs does, and we
  need all 56 bytes for stuff
-/
def mkELF64ProgramHeaderTableEntry 
  (bs : ByteArray) 
  (offset : Nat) 
  (isBigEndian : Bool)
  (h : bs.size - offset ≥ 0x38) :
  ELF64ProgramHeaderTableEntry := {
  elf64_p_type   := getUInt32from (offset + 0x00) (by omega), 
  elf64_p_flags  := getUInt32from (offset + 0x04) (by omega),
  elf64_p_offset := getUInt64from (offset + 0x08) (by omega),
  elf64_p_vaddr  := getUInt64from (offset + 0x10) (by omega),
  elf64_p_paddr  := getUInt64from (offset + 0x18) (by omega),
  elf64_p_filesz := getUInt64from (offset + 0x20) (by omega),
  elf64_p_memsz  := getUInt64from (offset + 0x28) (by omega),
  elf64_p_align  := getUInt64from (offset + 0x30) (by omega),
} where
  getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
  getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
  getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
