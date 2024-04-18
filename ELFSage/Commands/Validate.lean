import Cli
import ELFSage.Util.Cli

import ELFSage.Types.ELFHeader
import ELFSage.Types.File
import ELFSage.Types.Note
import ELFSage.Types.Dynamic
import ELFSage.Types.Relocation
import ELFSage.Types.SymbolTable

def mkRawProgramHeaderErrors
  [ELFHeader α]
  (eh : α)
  (bytes : ByteArray)
  : List (Except String RawProgramHeaderTableEntry) :=
  let shoffsets := (List.range (ELFHeader.e_phnum eh)).map λidx ↦ ELFHeader.e_phoff eh + ELFHeader.e_phentsize eh * idx
  let isBigendian := ELFHeader.isBigendian eh
  let is64Bit := ELFHeader.is64Bit eh
  List.map (λoffset ↦ mkRawProgramHeaderTableEntry? bytes is64Bit isBigendian offset) shoffsets

def mkRawSectionHeaderErrors
  [ELFHeader α]
  (eh : α)
  (bytes : ByteArray)
  : List (Except String RawSectionHeaderTableEntry) :=
  let shoffsets := (List.range (ELFHeader.e_shnum eh)).map λidx ↦ ELFHeader.e_shoff eh + ELFHeader.e_shentsize eh * idx
  let isBigendian := ELFHeader.isBigendian eh
  let is64Bit := ELFHeader.is64Bit eh
  List.map (λoffset ↦ mkRawSectionHeaderTableEntry? bytes is64Bit isBigendian offset) shoffsets

def getSectionNamesOrErr
  [ELFHeader α] (eh : α)
  [SectionHeaderTableEntry β] (sht: List (Except String β))
  (bytes : ByteArray)
  : Except String ELFStringTable :=
    let shstrndx := ELFHeader.e_shstrndx eh
    if h : shstrndx ≥ sht.length
    then .error $
       s!"the shstrndx from the elf header requests section {ELFHeader.e_shstrndx eh}, " ++
       s!"but the there are only {sht.length} section header table entries indicated."
    else match  sht[shstrndx] with
      | .error e => .error
           $ s!"the shstrndx from the elf header requests section {ELFHeader.e_shstrndx eh}, "
          ++ s!"but that section was unreadable, with the following error: {e}"
      | .ok shte => do
      let shstr_start := SectionHeaderTableEntry.sh_offset shte
      let shstr_end := shstr_start + SectionHeaderTableEntry.sh_size shte
      if shstr_end > bytes.size
      then .error $
          s!"The section header name string table offset 0x{Hex.toHex shstr_start}, and endpoint 0x{Hex.toHex shstr_end} " ++
          s!"runs off the end of the binary, which contains only 0x{Hex.toHex bytes.size} bytes."
      else .ok ⟨bytes.extract shstr_start shstr_end⟩

def printNotesErrs
  [ELFHeader α] (eh : α)
  (shte: RawSectionHeaderTableEntry)
  (sec: InterpretedSection)
  (idx : Nat)
  : IO Unit := recur (SectionHeaderTableEntry.sh_size shte) 0 1
  where
    alignTo4 n := n + (n % 4)
    recur space offset count :=
      match space with
      | 0 => pure ()
      | spaceminus + 1 => --we work with space-1 to automatically prove termination
        match mkRawNoteEntry?
          (sec.section_body)
          (ELFHeader.isBigendian eh)
          (ELFHeader.is64Bit eh)
          offset
        with
        | .error e => IO.println s!"Note {count} from section {idx} was damaged: {e}"
        | .ok ne => do
          let notesize := 0xc + alignTo4 (NoteEntry.note_name ne).size + alignTo4 (NoteEntry.note_desc ne).size
          if spaceminus - notesize ≥ 0xb
          then recur (spaceminus - (notesize - 1)) (offset + notesize) (count + 1)
          else pure ()

def printDynamicErrs
  [ELFHeader α] (eh : α)
  (shte: RawSectionHeaderTableEntry)
  (sec: InterpretedSection)
  (sidx : Nat)
  : IO Unit :=
    for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
      let offset := idx * SectionHeaderTableEntry.sh_entsize shte
      match mkRawDynamicEntry?
        sec.section_body
        (ELFHeader.is64Bit eh)
        (ELFHeader.isBigendian eh)
        offset
      with
      | .error e => IO.println s!"Dynamic Entry {idx} of section {sidx} was damaged: {e}"
      | .ok _ => pure ()

def printRelocationAErrs
  [ELFHeader α] (eh : α)
  (shte : RawSectionHeaderTableEntry)
  (sec : InterpretedSection)
  (sidx : Nat) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawRelocationA?
      (sec.section_body)
      (ELFHeader.is64Bit eh)
      (ELFHeader.isBigendian eh)
      offset
    with
    | .error e => IO.println s!"Relocation {idx} of section {sidx} was damaged: {e}"
    | .ok _ => pure ()

def printSymbolErrs
  [ELFHeader α] (eh : α)
  (shte: RawSectionHeaderTableEntry)
  (sec: InterpretedSection)
  (sidx : Nat) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry?
      sec.section_body
      (ELFHeader.is64Bit eh)
      (ELFHeader.isBigendian eh)
      offset
    with
    | .error e => IO.println s!"Symbol {idx} of section {sidx} was damaged: {e}"
    | .ok _ => pure ()


def runValidateCmd (p : Cli.Parsed) : IO UInt32 := do

  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  -- retrieve ELF header
  match mkRawELFHeader? bytes with
  | .error warn => do
    IO.println "The ELF Header can't be recovered, so nothing else is readable:"
    IO.println warn *> return 1
  | .ok elfheader => do

  -- retrieve other headers
  let phtEntries := mkRawProgramHeaderErrors elfheader bytes
  let shtEntries := mkRawSectionHeaderErrors elfheader bytes

  -- get section names
  let maybeNames := getSectionNamesOrErr elfheader shtEntries bytes
  let tryName (n: Nat) := match maybeNames with | .ok s => .some (s.stringAt n) | .error _ => .none

  match maybeNames with
  | .ok _ => pure ()
  | .error e => IO.println s!"The string table listing the section names is unavailable: {e}"

  let mut idx := 0

  for ent in phtEntries do
    idx := idx + 1

    match ent with
    | .error s => IO.println s!"Program Header Table Entry {idx}: {s}"
    | .ok phte =>

    match ProgramHeaderTableEntry.toSegment? phte bytes with
    | .error s => IO.println s!"Segment Associated with Program Header Table Entry {idx}: {s}"
    | .ok _ => pure ()

  idx := 0

  for ent in shtEntries do
    idx := idx + 1
    match ent with
    | .error s => IO.println s!"Section Header Table Entry {idx}: {s}"
    | .ok shte =>

    match SectionHeaderTableEntry.toSection? shte bytes (tryName idx)  with
    | .error s => IO.println s!"Section Associated with Section Header Table Entry {idx}: {s}"
    | .ok sec =>

    if SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_NOTE
    then printNotesErrs elfheader shte sec idx

    if SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC
    then printDynamicErrs elfheader shte sec idx

    if SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_SYMTAB
       ∨ SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_DYNSYM
    then printDynamicErrs elfheader shte sec idx

    if SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_RELA
    then printRelocationAErrs elfheader shte sec idx

  return 0
