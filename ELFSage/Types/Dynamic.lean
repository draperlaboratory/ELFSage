import ELFSage.Types.Sizes
import ELFSage.Util.ByteArray
import ELFSage.Constants.Dynamic

inductive DynamicUnion (α β : Type) where
  | d_val     : α → DynamicUnion α β 
  | d_ptr     : β → DynamicUnion α β 
  | d_ignored : ByteArray → DynamicUnion α β
  deriving Repr

inductive Dynamic.FieldInterpretation where
  | d_val     : FieldInterpretation
  | d_ptr     : FieldInterpretation
  | d_ignored : FieldInterpretation

inductive Dynamic.Tag where
  /-- [dt_null] marks the end of the dynamic array -/
  | dt_null : Dynamic.Tag
  /-- [dt_needed] holds the string table offset of a string containing the name of a needed library. -/
  | dt_needed : Dynamic.Tag
  /--[dt_pltrelsz] holds the size in bytes of relocation entries associated with the PLT. -/
  | dt_pltrelsz : Dynamic.Tag
  /-- [dt_pltgot] holds an address associated with the PLT or GOT. -/
  | dt_pltgot : Dynamic.Tag
  /-- [dt_hash] holds the address of a symbol-table hash. -/
  | dt_hash : Dynamic.Tag
  /-- [dt_strtab] holds the address of the string table. -/
  | dt_strtab : Dynamic.Tag
  /-- [dt_symtab] holds the address of a symbol table. -/
  | dt_symtab : Dynamic.Tag
  /-- [dt_rela] holds the address of a relocation table. -/
  | dt_rela : Dynamic.Tag
  /-- [dt_relasz] holds the size in bytes of the relocation table. -/
  | dt_relasz : Dynamic.Tag
  /-- [dt_relaent] holds the size in bytes of a relocation table entry. -/
  | dt_relaent : Dynamic.Tag
  /-- [dt_strsz] holds the size in bytes of the string table. -/
  | dt_strsz : Dynamic.Tag
  /-- [dt_syment] holds the size in bytes of a symbol table entry. -/
  | dt_syment : Dynamic.Tag
  /-- [dt_init] holds the address of the initialisation function. -/
  | dt_init : Dynamic.Tag
  /-- [dt_fini] holds the address of the finalisation function. -/
  | dt_fini : Dynamic.Tag
  /-- [dt_soname] holds the string table offset of a string containing the shared object name. -/
  | dt_soname : Dynamic.Tag
  /-- [dt_rpath] holds the string table offset of a string containing the library search path. -/
  | dt_rpath : Dynamic.Tag
  /-- [dt_symbolic] alters the linker's symbol resolution algorithm so that names are resolved first from the shared object file itself, rather than the executable file. -/
  | dt_symbolic : Dynamic.Tag
  /-- [dt_rel] is similar to [dt_rela] except its table has implicit addends. -/
  | dt_rel : Dynamic.Tag
  /-- [dt_relsz] holds the size in bytes of the [dt_rel] relocation table. -/
  | dt_relsz : Dynamic.Tag
  /-- [dt_relent] holds the size in bytes of a [dt_rel] relocation entry. -/
  | dt_relent : Dynamic.Tag
  /-- [dt_pltrel] specifies the type of relocation entry to which the PLT refers. -/
  | dt_pltrel : Dynamic.Tag
  /-- [dt_debug] is used for debugging and its purpose is not specified in the ABI. Programs using this entry are not ABI-conformant. -/
  | dt_debug : Dynamic.Tag
  /-- [dt_textrel] absence of this entry indicates that no relocation entry should
  a modification to a non-writable segment.  Otherwise, if present, one
  or more relocation entries may request modifications to a non-writable
  segment. -/
  | dt_textrel : Dynamic.Tag
  /-- [dt_jmprel]'s member holds the address of relocation entries associated with the PLT. -/
  | dt_jmprel : Dynamic.Tag
  /-- [dt_bindnow] instructs the linker to process all relocations for the object containing the entry before transferring control to the program. -/
  | dt_bindnow : Dynamic.Tag
  /-- [dt_init_array] holds the address to the array of pointers to initialisation functions. -/
  | dt_init_array : Dynamic.Tag
  /-- [dt_fini_array] holds the address to the array of pointers to finalisation functions. -/
  | dt_fini_array : Dynamic.Tag
  /-- [dt_init_arraysz] holds the size in bytes of the array of pointers to initialisation functions. -/
  | dt_init_arraysz : Dynamic.Tag
  /-- [dt_fini_arraysz] holds the size in bytes of the array of pointers to finalisation functions. -/
  | dt_fini_arraysz : Dynamic.Tag
  /-- [dt_runpath] holds an offset into the string table holding a string containing the library search path. -/
  | dt_runpath : Dynamic.Tag
  /-- [dt_flags] holds flag values specific to the object being loaded. -/
  | dt_flags : Dynamic.Tag
  /-- [dt_preinit_array] holds the address to the array of pointers of pre- initialisation functions. -/
  | dt_preinit_array : Dynamic.Tag
  /-- [dt_preinit_arraysz] holds the size in bytes of the array of pointers of pre-initialisation functions. -/
  | dt_preinit_arraysz : Dynamic.Tag
  /-- [dt_loos] and [dt_hios]: this inclusive range is reserved for OS-specific semantics. -/
  | dt_loos_hios (n :Nat) : Dynamic.Tag
  /-- [dt_loproc] and [dt_hiproc]: this inclusive range is reserved for processor specific semantics. -/
  | dt_loproc_hiproc (n: Nat) : Dynamic.Tag
  /-- [dt_loproc] and [dt_hiproc]: this inclusive range is reserved for processor specific semantics. -/
  | dt_unknown (n : Int) : Dynamic.Tag

section open Dynamic.Tag

def Dynamic.Tag.mkRawTag (x : Int) :  Dynamic.Tag :=
 if x == DT_NULL then .dt_null
 else if x == DT_NEEDED then .dt_needed
 else if x == DT_PLTRELSZ then .dt_pltrelsz
 else if x == DT_PLTGOT then .dt_pltgot
 else if x == DT_HASH then .dt_hash
 else if x == DT_STRTAB then .dt_strtab
 else if x == DT_SYMTAB then .dt_symtab
 else if x == DT_RELA then .dt_rela
 else if x == DT_RELASZ then .dt_relasz
 else if x == DT_RELAENT then .dt_relaent
 else if x == DT_STRSZ then .dt_strsz
 else if x == DT_SYMENT then .dt_syment
 else if x == DT_INIT then .dt_init
 else if x == DT_FINI then .dt_fini
 else if x == DT_SONAME then .dt_soname
 else if x == DT_RPATH then .dt_rpath
 else if x == DT_SYMBOLIC then .dt_symbolic
 else if x == DT_REL then .dt_rel
 else if x == DT_RELSZ then .dt_relsz
 else if x == DT_RELENT then .dt_relent
 else if x == DT_PLTREL then .dt_pltrel
 else if x == DT_DEBUG then .dt_debug
 else if x == DT_TEXTREL then .dt_textrel
 else if x == DT_JMPREL then .dt_jmprel
 else if x == DT_BINDNOW then .dt_bindnow
 else if x == DT_INIT_ARRAY then .dt_init_array
 else if x == DT_FINI_ARRAY then .dt_fini_array
 else if x == DT_INIT_ARRAYSZ then .dt_init_arraysz
 else if x == DT_FINI_ARRAYSZ then .dt_fini_arraysz
 else if x == DT_RUNPATH then .dt_runpath
 else if x == DT_FLAGS then .dt_flags
 else if x == DT_PREINIT_ARRAY then .dt_preinit_array
 else if x == DT_PREINIT_ARRAYSZ then .dt_preinit_arraysz
 else if x >= DT_LOOS ∧ DT_LOOS <= DT_LOOS then .dt_loos_hios x.toNat
 else if x >= DT_LOPROC ∧ x <= DT_LOOS then .dt_loproc_hiproc x.toNat
 else .dt_unknown x

end

def Dynamic.Tag.toFieldInterpretation 
  (dt : Dynamic.Tag)
  (os : Nat → Except String Dynamic.FieldInterpretation)
  (proc : Nat → Except String Dynamic.FieldInterpretation) 
  :  Except String Dynamic.FieldInterpretation := 
  match dt with
  | dt_null => .ok .d_ignored
  | dt_needed => .ok .d_val
  | dt_pltrelsz => .ok .d_val
  | dt_pltgot => .ok .d_ptr
  | dt_hash => .ok .d_ptr
  | dt_strtab => .ok .d_ptr
  | dt_symtab => .ok .d_ptr
  | dt_rela => .ok .d_ptr
  | dt_relasz => .ok .d_val
  | dt_relaent => .ok .d_val
  | dt_strsz => .ok .d_val
  | dt_syment => .ok .d_val
  | dt_init => .ok .d_ptr
  | dt_fini => .ok .d_ptr
  | dt_soname => .ok .d_val
  | dt_rpath => .ok .d_val
  | dt_symbolic => .ok .d_ignored
  | dt_rel => .ok .d_ptr
  | dt_relsz => .ok .d_val
  | dt_relent => .ok .d_val
  | dt_pltrel => .ok .d_val
  | dt_debug => .ok .d_ptr
  | dt_textrel => .ok .d_ignored
  | dt_jmprel => .ok .d_ptr
  | dt_bindnow => .ok .d_ignored
  | dt_init_array => .ok .d_ptr
  | dt_fini_array => .ok .d_ptr
  | dt_init_arraysz => .ok .d_val
  | dt_fini_arraysz => .ok .d_val
  | dt_runpath => .ok .d_val
  | dt_flags => .ok .d_val
  -- XXX: linksem makes the effect of DT_PREINIT_ARRAY (which is equal to
  -- DT_ENCODING, which is equal to 32) conditional on whether you're dealing
  -- with a shared object: 
  --   https://github.com/rems-project/linksem/blob/238f803b18e485eecbc550e0e2292257eaf7029b/src/elf_dynamic.lem#L364
  -- But other sources seem to say that it should be d_ptr
  --     https://www.sco.com/developers/gabi/2012-12-31/ch5.dynamic.html#dynamic_section
  --     https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html#scrolltoc
  | dt_preinit_array => .ok .d_ptr
  | dt_preinit_arraysz => .ok .d_val
  | dt_loos_hios (n :Nat) => os n
  | dt_loproc_hiproc (n: Nat) => proc n
  -- The rules for unknowns are mentioned in
  --     https://www.sco.com/developers/gabi/2012-12-31/ch5.dynamic.html#dynamic_section
  --     https://docs.oracle.com/cd/E23824_01/html/819-0690/chapter6-42444.html#scrolltoc
  -- oracle includes a fair number of tags not mentioned here or at linksem.
  | dt_unknown (x : Int) => if (x > DT_HIOS ∨ x < DT_LOPROC) ∧ x % 2 == 0
    then .ok .d_ptr
    else .error s!"unknown dynamic tag prevents interpretation of dt_un"  

structure RawDynamicEntry where
  d_tag : Int
  d_un  : DynamicUnion Nat Nat
  deriving Repr

structure ELF32DynamicEntry where
  d_tag  : elf32_sword
  d_un   : DynamicUnion elf32_word elf32_addr

def ELF32DynamicEntry.toRawDynamicEntry (de : ELF32DynamicEntry) : RawDynamicEntry := {
  d_tag := de.d_tag.toInt
  d_un := match de.d_un with
    | .d_val v => .d_val v.toNat
    | .d_ptr p => .d_ptr p.toNat
    | .d_ignored bs => .d_ignored bs
}

def mkELF32DynamicEntry?
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x8) 
  : Except String ELF32DynamicEntry := 
  let tagval := getUInt32from (offset + 0x0) (by omega)
  let rawTag := Dynamic.Tag.mkRawTag (⟨tagval⟩ : SInt32).toInt
  match rawTag.toFieldInterpretation 
      (λ_ => .error "os reserved tag, not implemented")
      (λ_ => .error "proc reserved tag, not implemented")
  with
  | .ok .d_val => .ok { d_tag := ⟨tagval⟩, d_un := .d_val $ getUInt32from (offset + 0x4) (by omega) }
  | .ok .d_ptr => .ok { d_tag := ⟨tagval⟩, d_un := .d_ptr $ getUInt32from (offset + 0x4) (by omega) }
  | .ok .d_ignored => .ok { d_tag := ⟨tagval⟩, d_un := .d_ignored $ bs.extract (offset + 0x4) (offset + 0x8) }
  | .error e => .error e
  where
    getUInt32from := if isBigEndian then bs.getUInt32BEfrom else bs.getUInt32LEfrom

structure ELF64DynamicEntry where
  d_tag  : elf64_sxword
  d_un   : DynamicUnion elf64_xword elf64_addr

def ELF64DynamicEntry.toRawDynamicEntry (de : ELF64DynamicEntry) : RawDynamicEntry := {
  d_tag := de.d_tag.toInt
  d_un := match de.d_un with
    | .d_val v => .d_val v.toNat
    | .d_ptr p => .d_ptr p.toNat
    | .d_ignored bs => .d_ignored bs
}

def mkELF64DynamicEntry?
  (isBigEndian : Bool)
  (bs : ByteArray)
  (offset : Nat)
  (h : bs.size - offset ≥ 0x10) 
  : Except String ELF64DynamicEntry := 
  let tagval := getUInt64from (offset + 0x00) (by omega)
  let rawTag := Dynamic.Tag.mkRawTag (⟨tagval⟩ : SInt64).toInt
  match rawTag.toFieldInterpretation 
      (λ_ => .error "os reserved tag, not implemented")
      (λ_ => .error "proc reserved tag, not implemented")
  with
  | .ok .d_val => .ok { d_tag := ⟨tagval⟩, d_un := .d_val $ getUInt64from (offset + 0x08) (by omega) }
  | .ok .d_ptr => .ok { d_tag := ⟨tagval⟩, d_un := .d_ptr $ getUInt64from (offset + 0x08) (by omega) }
  | .ok .d_ignored => .ok { d_tag := ⟨tagval⟩, d_un := .d_ignored $ bs.extract (offset + 0x08) (offset + 0x10) }
  | .error e => .error e
  where
    getUInt64from := if isBigEndian then bs.getUInt64BEfrom else bs.getUInt64LEfrom

def mkRawDynamicEntry?
  (bs : ByteArray)
  (is64Bit : Bool)
  (isBigendian : Bool)
  (offset : Nat)
  : Except String RawDynamicEntry := 
  match is64Bit with
  | true   => 
    if h : bs.size - offset ≥ 0x10
    then ELF64DynamicEntry.toRawDynamicEntry <$> mkELF64DynamicEntry? isBigendian bs offset h
    else throw $ err 0x10
  | false  => 
    if h : bs.size - offset ≥ 0x8
    then ELF32DynamicEntry.toRawDynamicEntry <$> mkELF32DynamicEntry? isBigendian bs offset h
    else throw $ err 0x08
  where
    err size := s! "Dynamic entry offset {offset} doesn't leave enough space for the entry, " ++
                s! "which requires {size} bytes."
