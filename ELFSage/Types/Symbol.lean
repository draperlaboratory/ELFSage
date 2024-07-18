import ELFSage.Types.SymbolTable
import ELFSage.Types.File

structure InterpretedSymbolTableEntry where
  /-- Index into the object file's string table -/
  st_name  : Nat
  /-- Specifies the symbol's type and binding attributes -/
  st_info  : Nat
  /-- Currently specifies the symbol's visibility -/
  st_other : Nat
  /-- Section header index symbol is defined with respect to -/
  st_shndx : Nat
  /-- Gives the value of the associated symbol -/
  st_value : Nat
  /-- Size of the associated symbol -/
  st_size  : Nat
  /-- Name of the symbol as a string, if available -/
  st_name_as_string : Option String
  /-- Body of the symbol, if available -/
  st_body : Option ByteArray

instance : SymbolTableEntry InterpretedSymbolTableEntry where
  st_name ste  := ste.st_name
  st_info ste  := ste.st_info
  st_other ste := ste.st_other
  st_shndx ste := ste.st_shndx
  st_value ste := ste.st_value
  st_size ste  := ste.st_size

/-- Given a symbol table entry, this finds the index and interpreted section
    corresponding to the section in which the symbol body occurs -/
def SymbolTableEntry.getTarget? [SymbolTableEntry α] (ste : α) (elffile : RawELFFile) : Except String (Nat × InterpretedSection) := do
    let target_secidx := SymbolTableEntry.st_shndx ste
    if target_secidx ∈ [0,0xfff1] then errSpecialSection
    let ⟨_, target_sec⟩ ← elffile.getRawSectionHeaderTableEntries[target_secidx]?.elim errNoSection Except.ok
    .ok ⟨target_secidx, target_sec⟩
    where errNoSection := .error "The section that symbol points to doesn't exist!"
          errSpecialSection := .error "special section, not implemented"

/-- A symbol's st_value field can mean either an offset in a section, in
  a relocatable file, or an intended virtual address in memory in an executable
  or a shared object file.
This computes the section offset for a symbol -/
def SymbolTableEntry.getSectionOffset?  [SymbolTableEntry α] (ste : α) (elffile : RawELFFile) : Except String Nat := do
  match ELFHeader.e_type_val elffile.getRawELFHeader with
  | .et_exec | .et_dyn => do
    let ⟨_, target_sec⟩ ← SymbolTableEntry.getTarget? ste elffile
    return SymbolTableEntry.st_value ste - target_sec.section_addr
  | _ => return SymbolTableEntry.st_value ste

/-- Given a symbol table entry, this finds the offset of the next symbol in the
same section, or the end of the section -/
def SymbolTableEntry.nextSymbolOffset?
  [SymbolTableEntry α]
  (ste : α)
  (elffile : RawELFFile)
  : Except String Nat := do
  let ⟨target_secidx, target_sec⟩ ← SymbolTableEntry.getTarget? ste elffile
  let ⟨symshte, symsec⟩ ← elffile.getSymbolTable?
  let firstByte ← SymbolTableEntry.getSectionOffset? ste elffile
  let mut candidate := target_sec.section_size
  for idx in [:SectionHeaderTableEntry.sh_size symshte / SectionHeaderTableEntry.sh_entsize symshte] do
    match mkRawSymbolTableEntry?
      symsec.section_body elffile.is64Bit elffile.isBigendian
      (idx * SectionHeaderTableEntry.sh_entsize symshte)
    with
    | .error _ => pure ()
    | .ok otherste =>
    if SymbolTableEntry.st_shndx otherste ≠ target_secidx then continue
    let sectionOffset ← SymbolTableEntry.getSectionOffset? otherste elffile
    if sectionOffset < candidate && sectionOffset > firstByte
    then candidate := sectionOffset
    else candidate := candidate
  return candidate

def SymbolTableEntry.toBody?
  [SymbolTableEntry α]
  (ste : α)
  (elffile : RawELFFile)
  : Except String ByteArray := do
  let ⟨_, target_sec⟩ ← SymbolTableEntry.getTarget? ste elffile
  let firstByte ← SymbolTableEntry.getSectionOffset? ste elffile
  let lastByte ← if SymbolTableEntry.st_size ste = 0
    then SymbolTableEntry.nextSymbolOffset? ste elffile
    else pure $ firstByte + SymbolTableEntry.st_size ste
  if lastByte > target_sec.section_body.size
  then .error "the address calculated for this symbol doesn't correspond to anything in the binary"
  else .ok $ target_sec.section_body.extract firstByte lastByte
