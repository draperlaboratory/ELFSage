import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

-- TODO: should the ID field be a separate structure?
structure ELF64Header where
   elf64_ident    : List UInt8    -- Identification field
   elf64_type     : elf64_half    -- The object file type
   elf64_machine  : elf64_half    -- Required machine architecture
   elf64_version  : elf64_word    -- Object file version
   elf64_entry    : elf64_addr    -- Virtual address for transfer of control
   elf64_phoff    : elf64_off     -- Program header table offset in bytes
   elf64_shoff    : elf64_off     -- Section header table offset in bytes
   elf64_flags    : elf64_word    -- Processor-specific flags
   elf64_ehsize   : elf64_half    -- ELF header size in bytes
   elf64_phentsize: elf64_half    -- Program header table entry size in bytes
   elf64_phnum    : elf64_half    -- Number of entries in program header table
   elf64_shentsize: elf64_half    -- Section header table entry size in bytes
   elf64_shnum    : elf64_half    -- Number of entries in section header table
   elf64_shstrndx : elf64_half    -- Section header table entry for section name string table
   deriving Repr

/-- A simple parser for extracting an ELF header, just a test, no validation -/
def mkELF64Header (bs : ByteArray) (h : bs.size >= 64) : ELF64Header := { 
  elf64_ident     := (bs.extract 0x0 0x9).toList
  elf64_type      := getUInt16from bs 0x10 (by omega),
  elf64_machine   := getUInt16from bs 0x12 (by omega),
  elf64_version   := getUInt32from bs 0x14 (by omega),
  elf64_entry     := getUInt64from bs 0x18 (by omega),
  elf64_phoff     := getUInt64from bs 0x20 (by omega),
  elf64_shoff     := getUInt64from bs 0x28 (by omega),
  elf64_flags     := getUInt32from bs 0x30 (by omega),
  elf64_ehsize    := getUInt16from bs 0x34 (by omega),
  elf64_phentsize := getUInt16from bs 0x36 (by omega),
  elf64_phnum     := getUInt16from bs 0x38 (by omega),
  elf64_shentsize := getUInt16from bs 0x3A (by omega),
  elf64_shnum     := getUInt16from bs 0x3C (by omega),
  elf64_shstrndx  := getUInt16from bs 0x3E (by omega),
} where 
  isBigEndian := bs.get ⟨0x5,by omega⟩ == 1
  getUInt16from := if isBigEndian then ByteArray.getUInt16BEfrom else ByteArray.getUInt16LEfrom
  getUInt32from := if isBigEndian then ByteArray.getUInt32BEfrom else ByteArray.getUInt32LEfrom
  getUInt64from := if isBigEndian then ByteArray.getUInt64BEfrom else ByteArray.getUInt64LEfrom
