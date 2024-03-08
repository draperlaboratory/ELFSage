import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray

structure ELF64SectionHeaderTableEntry where
  /-- Name of the section -/
  elf64_sh_name      : elf64_word
  /-- Type of the section and its semantics -/
  elf64_sh_type      : elf64_word
  /-- Flags associated with the section -/
  elf64_sh_flags     : elf64_xword
  /-- Address of first byte of section in memory image -/
  elf64_sh_addr      : elf64_addr
  /-- Offset from beginning of file of first byte of section -/
  elf64_sh_offset    : elf64_off
  /-- Section size in bytes -/
  elf64_sh_size      : elf64_xword
  /-- Section header table index link -/
  elf64_sh_link      : elf64_word
  /-- Extra information, contents depends on type of section -/
  elf64_sh_info      : elf64_word
  /-- Alignment constraints for section -/
  elf64_sh_addralign : elf64_xword
  /-- Size of each entry in table, if section is composed of entries. Otherwise zero. -/
  elf64_sh_entsize   : elf64_xword
  deriving Repr

/- XXX: Should these be a big enum? -/

/-- Marks the section header as being inactive. -/
def SHT_NULL : elf64_word := 0
/-- Section holds information defined by the program. -/
def SHT_PROGBITS : elf64_word := 1
/-- Section holds a symbol table. The symtab provides a place for link editing. -/
def SHT_SYMTAB : elf64_word := 2
/-- Section holds a symbol table. The dynsym section holds a minimal set of
    dynamic linking symbols -/
def SHT_DYNSYM : elf64_word := 11
/-- Section holds a string table -/
def SHT_STRTAB : elf64_word := 3
/-- Section holds relocation entries with explicit addends.  An object file may
    have multiple section of this type. -/
def SHT_RELA : elf64_word := 4
/-- Section holds a symbol hash table.  An object file may only have a single
    hash table. -/
def SHT_HASH : elf64_word := 5
/-- Section holds information for dynamic linking.  An object file may only have
    a single dynamic section. -/
def SHT_DYNAMIC : elf64_word := 6
/-- Section holds information that marks the file in some way. -/
def SHT_NOTE : elf64_word := 7
/-- Section occupies no space in the file but otherwise resembles a progbits
    section. -/
def SHT_NOBITS : elf64_word := 8
/-- Section holds relocation entries without explicit addends.  An object file
    may have multiple section of this type. -/
def SHT_REL : elf64_word := 9
/-- Section type is reserved but has an unspecified meaning. -/
def SHT_SHLIB : elf64_word := 10
/-- Section contains an array of pointers to initialisation functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def SHT_INIT_ARRAY : elf64_word := 14
/-- Section contains an array of pointers to termination functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def SHT_FINI_ARRAY : elf64_word := 15
/-- Section contains an array of pointers to initialisation functions that are
    invoked before all other initialisation functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def SHT_PREINIT_ARRAY : elf64_word := 16
/-- Section defines a section group, i.e. a set of sections that are related and
    must be treated especially by the linker.  May only appear in relocatable
    objects. -/
def SHT_GROUP : elf64_word := 17
/-- Section is associated with sections of type SHT_SYMTAB and is required if
    any of the section header indices referenced by that symbol table contains
    the escape value SHN_XINDEX. -/
def SHT_SYMTAB_SHNDX : elf64_word := 18

def mkELF64SectionHeaderTableEntry
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x40) :
  ELF64SectionHeaderTableEntry := {
    elf64_sh_name      := getUInt32from (offset + 0x00) (by omega),
    elf64_sh_type      := getUInt32from (offset + 0x04) (by omega),
    elf64_sh_flags     := getUInt64from (offset + 0x08) (by omega),
    elf64_sh_addr      := getUInt64from (offset + 0x10) (by omega),
    elf64_sh_offset    := getUInt64from (offset + 0x18) (by omega),
    elf64_sh_size      := getUInt64from (offset + 0x20) (by omega),
    elf64_sh_link      := getUInt32from (offset + 0x28) (by omega),
    elf64_sh_info      := getUInt32from (offset + 0x2C) (by omega),
    elf64_sh_addralign := getUInt64from (offset + 0x30) (by omega),
    elf64_sh_entsize   := getUInt64from (offset + 0x38) (by omega),
  } where
    getUInt16from := if isBigEndian then bs.getUInt16BEfrom else bs.getUInt16LEfrom
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom
