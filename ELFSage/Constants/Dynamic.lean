/-- [dt_null] marks the end of the dynamic array -/
def Dynamic.Tag.DT_NULL : Nat := 0
/-- [dt_needed] holds the string table offset of a string containing the name of a needed library. -/
def Dynamic.Tag.DT_NEEDED : Nat := 1
/--[dt_pltrelsz] holds the size in bytes of relocation entries associated with the PLT. -/
def Dynamic.Tag.DT_PLTRELSZ : Nat := 2
/-- [dt_pltgot] holds an address associated with the PLT or GOT. -/
def Dynamic.Tag.DT_PLTGOT : Nat := 3
/-- [dt_hash] holds the address of a symbol-table hash. -/
def Dynamic.Tag.DT_HASH : Nat := 4
/-- [dt_strtab] holds the address of the string table. -/
def Dynamic.Tag.DT_STRTAB : Nat := 5
/-- [dt_symtab] holds the address of a symbol table. -/
def Dynamic.Tag.DT_SYMTAB : Nat := 6
/-- [dt_rela] holds the address of a relocation table. -/
def Dynamic.Tag.DT_RELA : Nat := 7
/-- [dt_relasz] holds the size in bytes of the relocation table. -/
def Dynamic.Tag.DT_RELASZ : Nat := 8
/-- [dt_relaent] holds the size in bytes of a relocation table entry. -/
def Dynamic.Tag.DT_RELAENT : Nat := 9
/-- [dt_strsz] holds the size in bytes of the string table. -/
def Dynamic.Tag.DT_STRSZ : Nat := 10
/-- [dt_syment] holds the size in bytes of a symbol table entry. -/
def Dynamic.Tag.DT_SYMENT : Nat := 11
/-- [dt_init] holds the address of the initialisation function. -/
def Dynamic.Tag.DT_INIT : Nat := 12
/-- [dt_fini] holds the address of the finalisation function. -/
def Dynamic.Tag.DT_FINI : Nat := 13
/-- [dt_soname] holds the string table offset of a string containing the shared object name. -/
def Dynamic.Tag.DT_SONAME : Nat := 14
/-- [dt_rpath] holds the string table offset of a string containing the library search path. -/
def Dynamic.Tag.DT_RPATH : Nat := 15
/-- [dt_symbolic] alters the linker's symbol resolution algorithm so that names are resolved first from the shared object file itself, rather than the executable file. -/
def Dynamic.Tag.DT_SYMBOLIC : Nat := 16
/-- [dt_rel] is similar to [dt_rela] except its table has implicit addends. -/
def Dynamic.Tag.DT_REL : Nat := 17
/-- [dt_relsz] holds the size in bytes of the [dt_rel] relocation table. -/
def Dynamic.Tag.DT_RELSZ : Nat := 18
/-- [dt_relent] holds the size in bytes of a [dt_rel] relocation entry. -/
def Dynamic.Tag.DT_RELENT : Nat := 19
/-- [dt_pltrel] specifies the type of relocation entry to which the PLT refers. -/
def Dynamic.Tag.DT_PLTREL : Nat := 20
/-- [dt_debug] is used for debugging and its purpose is not specified in the ABI. Programs using this entry are not ABI-conformant. -/
def Dynamic.Tag.DT_DEBUG : Nat := 21
/-- [dt_textrel] absence of this entry indicates that no relocation entry should
cause a modification to a non-writable segment.  Otherwise, if present, one
or more relocation entries may request modifications to a non-writable
segment. -/
def Dynamic.Tag.DT_TEXTREL : Nat := 22
/-- [dt_jmprel]'s member holds the address of relocation entries associated with the PLT. -/
def Dynamic.Tag.DT_JMPREL : Nat := 23
/-- [dt_bindnow] instructs the linker to process all relocations for the object containing the entry before transferring control to the program. -/
def Dynamic.Tag.DT_BINDNOW : Nat := 24
/-- [dt_init_array] holds the address to the array of pointers to initialisation functions. -/
def Dynamic.Tag.DT_INIT_ARRAY : Nat := 25
/-- [dt_fini_array] holds the address to the array of pointers to finalisation functions. -/
def Dynamic.Tag.DT_FINI_ARRAY : Nat := 26
/-- [dt_init_arraysz] holds the size in bytes of the array of pointers to initialisation functions. -/
def Dynamic.Tag.DT_INIT_ARRAYSZ : Nat := 27
/-- [dt_fini_arraysz] holds the size in bytes of the array of pointers to finalisation functions. -/
def Dynamic.Tag.DT_FINI_ARRAYSZ : Nat := 28
/-- [dt_runpath] holds an offset into the string table holding a string containing the library search path. -/
def Dynamic.Tag.DT_RUNPATH : Nat := 29
/-- [dt_flags] holds flag values specific to the object being loaded. -/
def Dynamic.Tag.DT_FLAGS : Nat := 30
/-- [dt_encoding] is a special value, not corresponding to an actual tag (its value is equal to the value of dt_preinit_array). Tags above DT_ENCODING have special sematnics: https://www.sco.com/developers/gabi/2012-12-31/ch5.dynamic.html#dynamic_section -/
def Dynamic.Tag.DT_ENCODING : Nat := 32
/-- [dt_preinit_array] holds the address to the array of pointers of pre- initialisation functions. -/
def Dynamic.Tag.DT_PREINIT_ARRAY : Nat := 32
/-- [dt_preinit_arraysz] holds the size in bytes of the array of pointers of pre-initialisation functions. -/
def Dynamic.Tag.DT_PREINIT_ARRAYSZ : Nat := 33
/-- [dt_loos] and [dt_hios]: this inclusive range is reserved for OS-specific semantics. -/
def Dynamic.Tag.DT_LOOS : Nat := 0x6000000D
/-- [dt_loos] and [dt_hios]: this inclusive range is reserved for OS-specific semantics. -/
def Dynamic.Tag.DT_HIOS : Nat := 0x6ffff000
/-- [dt_loproc] and [dt_hiproc]: this inclusive range is reserved for processor specific semantics. -/
def Dynamic.Tag.DT_LOPROC : Nat := 0x70000000 
/-- [dt_loproc] and [dt_hiproc]: this inclusive range is reserved for processor specific semantics. -/
def Dynamic.Tag.DT_HIPROC : Nat := 0x7fffffff
