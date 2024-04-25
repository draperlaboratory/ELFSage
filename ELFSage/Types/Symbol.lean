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

def SymbolTableEntry.toBody?
  [SymbolTableEntry α]
  (ste : α)
  (elffile : RawELFFile)
  : Except String ByteArray := do
  let target_secidx := SymbolTableEntry.st_shndx ste
  if target_secidx ∈ [0,0xfff1]
  then .error "special section, not implemented"
  else match elffile.getRawSectionHeaderTableEntries[target_secidx]? with
    | .none => .error "The section that symbol points to doesn't exist!"
    | .some ⟨_, target_sec⟩ => do
    let valueCorrection :=
      -- in executables, the symbol value means virtual address. In relocatable
      -- files, it mean offset in the section.
      match ELFHeader.e_type_val elffile with
      | .et_exec => target_sec.section_addr
      | _ => 0
    let firstByte := SymbolTableEntry.st_value ste - valueCorrection
    let lastByte ← if SymbolTableEntry.st_size ste = 0
      then nextSymReference target_secidx target_sec valueCorrection firstByte
      else pure $ firstByte + SymbolTableEntry.st_size ste
    if lastByte > target_sec.section_body.size
    then .error "the address calculated for this symbol doesn't correspond to anything in the binary"
    else .ok $ target_sec.section_body.extract firstByte lastByte
  where nextSymReference target_secidx target_sec vc fb := do
    match elffile.getSymbolTable? with
    | .none => .error "No symbol table present, no st_size given, can't guess byte range"
    | .some ⟨symshte, symsec⟩ => do
    let mut candidate := target_sec.section_size
    for idx in [:SectionHeaderTableEntry.sh_size symshte / SectionHeaderTableEntry.sh_entsize symshte] do
      let offset := idx * SectionHeaderTableEntry.sh_entsize symshte
      match mkRawSymbolTableEntry?
        symsec.section_body
        (ELFHeader.is64Bit elffile)
        (ELFHeader.isBigendian elffile)
        offset
      with
      | .error _ => pure ()
      | .ok ste =>
      if SymbolTableEntry.st_shndx ste = target_secidx &&
         SymbolTableEntry.st_value ste - vc < candidate &&
         SymbolTableEntry.st_value ste - vc > fb
      then candidate := SymbolTableEntry.st_value ste - vc
      else candidate := candidate
    return candidate
