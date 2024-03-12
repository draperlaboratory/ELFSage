/-- Marks the section header as being inactive. -/
def ELFSectionHeaderTableEntry.Type.SHT_NULL : Nat := 0
/-- Section holds information defined by the program. -/
def ELFSectionHeaderTableEntry.Type.SHT_PROGBITS : Nat := 1
/-- Section holds a symbol table. The symtab provides a place for link editing. -/
def ELFSectionHeaderTableEntry.Type.SHT_SYMTAB : Nat := 2
/-- Section holds a string table -/
def ELFSectionHeaderTableEntry.Type.SHT_STRTAB : Nat := 3
/-- Section holds relocation entries with explicit addends.  An object file may
    have multiple section of this type. -/
def ELFSectionHeaderTableEntry.Type.SHT_RELA : Nat := 4
/-- Section holds a symbol hash table.  An object file may only have a single
    hash table. -/
def ELFSectionHeaderTableEntry.Type.SHT_HASH : Nat := 5
/-- Section holds information for dynamic linking.  An object file may only have
    a single dynamic section. -/
def ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC : Nat := 6
/-- Section holds information that marks the file in some way. -/
def ELFSectionHeaderTableEntry.Type.SHT_NOTE : Nat := 7
/-- Section occupies no space in the file but otherwise resembles a progbits
    section. -/
def ELFSectionHeaderTableEntry.Type.SHT_NOBITS : Nat := 8
/-- Section holds relocation entries without explicit addends.  An object file
    may have multiple section of this type. -/
def ELFSectionHeaderTableEntry.Type.SHT_REL : Nat := 9
/-- Section type is reserved but has an unspecified meaning. -/
def ELFSectionHeaderTableEntry.Type.SHT_SHLIB : Nat := 10
/-- Section holds a symbol table. The dynsym section holds a minimal set of
    dynamic linking symbols -/
def ELFSectionHeaderTableEntry.Type.SHT_DYNSYM : Nat := 11
/-- Section contains an array of pointers to initialisation functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def ELFSectionHeaderTableEntry.Type.SHT_INIT_ARRAY : Nat := 14
/-- Section contains an array of pointers to termination functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def ELFSectionHeaderTableEntry.Type.SHT_FINI_ARRAY : Nat := 15
/-- Section contains an array of pointers to initialisation functions that are
    invoked before all other initialisation functions.  Each
    pointer is taken as a parameterless function with a void return type. -/
def ELFSectionHeaderTableEntry.Type.SHT_PREINIT_ARRAY : Nat := 16
/-- Section defines a section group, i.e. a set of sections that are related and
    must be treated especially by the linker.  May only appear in relocatable
    objects. -/
def ELFSectionHeaderTableEntry.Type.SHT_GROUP : Nat := 17
/-- Section is associated with sections of type SHT_SYMTAB and is required if
    any of the section header indices referenced by that symbol table contains
    the escape value SHN_XINDEX. -/
def ELFSectionHeaderTableEntry.Type.SHT_SYMTAB_SHNDX : Nat := 18

/- XXX: This is what the rems-project uses. some other values, like SHT_NUM, SHT_LOOS
   seem to be missing -/
