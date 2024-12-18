import Cli
import ELFSage.Util.Cli
import ELFSage.Util.IO
import ELFSage.Types.File
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.SymbolTable
import ELFSage.Types.StringTable
import ELFSage.Types.Dynamic
import ELFSage.Types.Note
import ELFSage.Types.Relocation
import ELFSage.Types.Symbol

def checkImplemented (p: Cli.Parsed) : Except String Unit := do
  let unimplemented :=
    [ "a", "all"
    , "g", "section-groups"
    , "t", "section-details"
    , "lto-syms"
    , "sym-base"
    , "C", "demangle"
    , "u", "unwind"
    , "V", "version-info"
    , "A", "arch-specific"
    , "c", "archive-index"
    , "D", "use-dynamic"
    , "L", "lint"
    , "R", "relocated-dump"
    , "z", "decompress"
    ]
  for flag in unimplemented do
    if p.hasFlag flag
    then throw s!"The flag --{flag} isn't implemented yet, sorry!"

  return ()

/- Prints all the symbols in the section with header `sectionHeaderEnt` -/
def printSymbolsForSection
  (elffile : RawELFFile)
  (shte: RawSectionHeaderTableEntry)
  (sec : InterpretedSection) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Symbol {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry?
      sec.section_body elffile.is64Bit elffile.isBigendian
      offset
    with
    | .error warn => IO.println warn
    | .ok ste =>
      match RawELFFile.symbolNameByLinkAndOffset elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste) with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}\n"
      IO.println $ repr ste

/- gets the symbol in the section with header `sectionHeaderEnt` with index symidx -/
def getSymbolNameInSection
  (elffile : RawELFFile)
  (shte : RawSectionHeaderTableEntry)
  (sec : InterpretedSection)
  (symidx : Nat)
  : Except String String :=
    let offset := symidx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry?
      sec.section_body
      elffile.is64Bit
      elffile.isBigendian
      offset
    with
    | .error warn => .error warn
    | .ok ste => RawELFFile.symbolNameByLinkAndOffset
      elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste)

--TODO find? might be more appropriate than filter here.

def printSymbolsForSectionType (elffile: RawELFFile) (ent_type : Nat) :=
  let ofType := elffile.getRawSectionHeaderTableEntries.filter $ λ⟨shte, _⟩↦
    SectionHeaderTableEntry.sh_type shte == ent_type
  ofType.forM $ λ⟨shte, sec⟩ ↦ printSymbolsForSection elffile shte sec

def printStringsForSectionIdx (elffile : RawELFFile) (idx : Nat) :=
  match elffile.getRawSectionHeaderTableEntries[idx]? with
  | .none => IO.println s!"There doesn't appear to be a section header {idx}"
  | .some ⟨_, sec⟩ => for byte in sec.section_body  do
      if byte == 0 then IO.print '\n' else IO.print (Char.ofNat byte.toNat)

def msecByName (elffile : RawELFFile) (name : String) : IO (Option (RawSectionHeaderTableEntry × InterpretedSection)) :=
  match elffile.getSectionHeaderStringTable? with
  | .error err => IO.println err *> return none
  | .ok (_,shstrtab_sec) =>
  let shstrtab : ELFStringTable := ⟨shstrtab_sec.section_body⟩
  let offset := shstrtab.stringToOffset name
  let findPred : RawSectionHeaderTableEntry × InterpretedSection → Bool := (λent => SectionHeaderTableEntry.sh_name ent.fst == offset)
  return (elffile.getRawSectionHeaderTableEntries.find? findPred)

def printStringsForSectionName (elffile : RawELFFile) (name : String) := do
  match (← msecByName elffile name) with
  | .none => IO.println s!"There doesn't appear to be a section header named {name}"
  | .some ⟨_, sec⟩ => for byte in sec.section_body  do
      if byte == 0 then IO.print '\n' else IO.print (Char.ofNat byte.toNat)

def printHexForSectionIdx (elffile : RawELFFile) (idx : Nat) :=
  match elffile.getRawSectionHeaderTableEntries[idx]? with
  | .none => IO.println s!"There doesn't appear to be a section header {idx}"
  | .some ⟨_, sec⟩ => dumpBytesAsHex sec.section_body

def printHexForSectionName (elffile : RawELFFile) (name : String) := do
  match (← msecByName elffile name) with
  | .none => IO.println s!"There doesn't appear to be a section header named {name}"
  | .some ⟨_, sec⟩ => dumpBytesAsHex sec.section_body

def printHexForSymbolIdx (elffile : RawELFFile) (idx : Nat) :=
  match do
    let ⟨symshte, symsec⟩ ← elffile.getSymbolTable?
    let offset := idx * SectionHeaderTableEntry.sh_entsize symshte
    let ste ← mkRawSymbolTableEntry?
      symsec.section_body
      elffile.is64Bit
      elffile.isBigendian
      offset
    SymbolTableEntry.toBody? ste elffile
  with
  | .error warn => IO.println warn
  | .ok bytes => dumpBytesAsHex bytes

def printDynamics (elffile : RawELFFile) :=
  let dynamics := elffile.getRawSectionHeaderTableEntries.filter $ λsec ↦
    SectionHeaderTableEntry.sh_type sec.fst == ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC
  for ⟨shte, sec⟩ in dynamics do
    for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Dynamic Entry {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawDynamicEntry?
      sec.section_body
      elffile.is64Bit
      elffile.isBigendian
      offset
    with
    | .error e => IO.println s!"warning: {e}"
    | .ok dynamicEnt => IO.println $ repr dynamicEnt

-- should use this for both SHT_NOTE sections and PT_NOTE segments
def printNotes
  (elffile: RawELFFile)
  (shte: RawSectionHeaderTableEntry)
  (sec: InterpretedSection)
  : IO Unit := recur (SectionHeaderTableEntry.sh_size shte) 0
  where
    alignTo4 n := n + (n % 4)
    recur space offset :=
      match space with
      | 0 => pure ()
      | spaceminus + 1 => --we work with space-1 to automatically prove termination
        match mkRawNoteEntry?
          sec.section_body
          elffile.isBigendian
          elffile.is64Bit
          offset
        with
        | .error e => IO.println e
        | .ok ne => do
          IO.println $ repr ne
          let notesize := 0xc + alignTo4 (NoteEntry.note_name ne).size + alignTo4 (NoteEntry.note_desc ne).size
          if spaceminus - notesize ≥ 0xb
          then recur (spaceminus - (notesize - 1)) (offset + notesize)
          else pure ()

def printNoteSections (elffile: RawELFFile) :=
  for ⟨shte, sec⟩ in elffile.getRawSectionHeaderTableEntries do
    if SectionHeaderTableEntry.sh_type shte == ELFSectionHeaderTableEntry.Type.SHT_NOTE then
      match sec.section_name_as_string with
      | .some name => IO.print s!"Notes from {name}\n"
      | .none => IO.print s!"Notes from unnamed section\n"
      printNotes elffile shte sec

def printRelocationA
  (elffile : RawELFFile)
  (shte : RawSectionHeaderTableEntry)
  (sec : InterpretedSection) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Relocation {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawRelocationA?
      sec.section_body
      elffile.is64Bit
      elffile.isBigendian
      offset
    with
    | .error warn => IO.println warn
    | .ok ra =>
      let r_sym := if elffile.is64Bit
        then (RelocationA.ra_info ra) / 2^32
        else (RelocationA.ra_info ra) / 2^8
      match elffile.getRawSectionHeaderTableEntries[linkedSymbolNames]? with
      | .some ⟨shte, sec⟩ => match getSymbolNameInSection elffile shte sec r_sym with
        | .ok name => IO.print s!"{name}\n"
        | .error warn => IO.print s!"??? - {warn}\n"
      | .none => IO.print s!"??? - can't locate string table of symbol names\n"
      IO.println $ repr ra
  where
    linkedSymbolNames := SectionHeaderTableEntry.sh_link shte

def printRelocationSections (elffile: RawELFFile) :=
  let relocations := elffile.getRawSectionHeaderTableEntries.filter $ λsec ↦
    SectionHeaderTableEntry.sh_type sec.fst == ELFSectionHeaderTableEntry.Type.SHT_RELA
  for ⟨shte, sec⟩ in relocations do
      match sec.section_name_as_string with
      | .some name => IO.print s!"Relocations from {name}\n"
      | .none => IO.print s!"Relocations from unnamed section\n"
      printRelocationA elffile shte sec

private def printFileHeader (eh : RawELFHeader) := do
  IO.println $ toString eh

private def printSectionHeaders (elffile : RawELFFile) := do
  IO.println $ RawELFFile.sectionHeadersToString elffile

private def printProgramHeaders (elffile : RawELFFile) := do
  IO.println $ RawELFFile.programHeadersToString elffile

private def printHeaders (elffile : RawELFFile) := do
  IO.println $ RawELFFile.headersToString elffile

def runReadCmd (p : Cli.Parsed): IO UInt32 := do

  match checkImplemented p with
  | .error warn => IO.println warn *> return 1
  | .ok _ => do

  if p.flags.size == 0 then do
    p.printHelp
    return 1

  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFFile? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok elffile => do

  let elfheader := elffile.getRawELFHeader

  for flag in p.flags do
    match flag.flag.longName with
    | "file-header" => printFileHeader elfheader
    | "headers" => printHeaders elffile
    | "program-headers" => printProgramHeaders elffile
    | "segments" => printProgramHeaders elffile
    | "section-headers" => printSectionHeaders elffile
    | "sections" => printSectionHeaders elffile
    | "dynamic" => printDynamics elffile
    | "dyn-syms" =>
      let type := ELFSectionHeaderTableEntry.Type.SHT_DYNSYM;
      printSymbolsForSectionType elffile type
    | "syms" =>
      let symtab := ELFSectionHeaderTableEntry.Type.SHT_SYMTAB
      printSymbolsForSectionType elffile symtab
      --TODO fallback to DYNSYM when SYMTAB isn't present
    | "string-dump" => match flag.as? Nat with
      | some idx => printStringsForSectionIdx elffile idx
      | none => match flag.as? String with
        | some name => printStringsForSectionName elffile name
        | none => IO.println "couldn't parse section number provided for string dump"
    | "hex-dump" => match flag.as? Nat with
      | some idx => printHexForSectionIdx elffile idx
      | none => match flag.as? String with
        | some name => printHexForSectionName elffile name
        | none => IO.println "couldn't parse section provided for hex dump"
    | "sym-dump" => match flag.as? Nat with
      | none => IO.println "couldn't parse symbol number provided for hex dump"
      | some idx => printHexForSymbolIdx elffile idx
    | "notes" => printNoteSections elffile
    | "relocs" => printRelocationSections elffile
    | _ => IO.println $ "unrecognized flag: " ++ flag.flag.longName

  return 0
