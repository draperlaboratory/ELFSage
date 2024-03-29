import Cli
import ELFSage.Util.Cli
import ELFSage.Types.File
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Constants.SectionHeaderTable
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.SymbolTable
import ELFSage.Types.StringTable
import ELFSage.Types.Dynamic
import ELFSage.Types.Note
import ELFSage.Types.Relocation

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

def printProgramHeaders (ef : RawELFFile) := do
  let headers := ef.getRawProgramHeaderTableEntries
  let mut idx := 0
  for header in headers do
    IO.println s!"\nProgram Header {idx}\n"
    IO.println $ repr header.fst
    idx := idx + 1

def getSectionByIndex (eh: RawELFHeader) (bytes : ByteArray) (idx: Nat) : Except String RawSectionHeaderTableEntry :=
  let header_offset := ELFHeader.e_shoff eh + (idx * ELFHeader.e_shentsize eh)
  match mkRawSectionHeaderTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) header_offset with
  | .error warn => .error s!"unable to locate the section header table at index {idx}, {warn}."
  | .ok s => .ok s

def symbolNameByLinkAndOffset 
  (elffile : RawELFFile)
  (linkIdx: Nat) 
  (offset : Nat) 
  : Except String String := 
  match elffile.getRawSectionHeaderTableEntries[linkIdx]? with
  | .none => .error "The section the symbol table references for names doesn't exist"
  | .some ⟨_, sec⟩ =>
    let stringtable : ELFStringTable := ⟨sec.section_body⟩
    pure $ stringtable.stringAt offset

def printSectionHeaders (elffile : RawELFFile) := do
  let headers := elffile.getRawSectionHeaderTableEntries
  let mut idx := 0
  for header in headers do
    let name := match header.snd.section_name_as_string with | .some s => s | _ => "no name"
    IO.println s!"\nSection Header {idx} -- {name} \n"
    IO.println $ repr header.fst
    idx := idx + 1

def printHeaders (elffile : RawELFFile) := do
  IO.println $ repr elffile.getRawELFHeader
  printProgramHeaders elffile
  printSectionHeaders elffile

/- Prints all the symbols in the section with header `sectionHeaderEnt` -/
def printSymbolsForSection 
  (elffile : RawELFFile) 
  (shte: RawSectionHeaderTableEntry) 
  (sec : InterpretedSection) := 
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Symbol {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry? sec.section_body is64Bit isBigEndian offset with
    | .error warn => IO.println warn
    | .ok ste => 
      match symbolNameByLinkAndOffset elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste) with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}\n"
      IO.println $ repr ste
  where
    isBigEndian := ELFHeader.isBigendian $ elffile.getRawELFHeader
    is64Bit := ELFHeader.is64Bit $ elffile.getRawELFHeader

/- gets the symbol in the section with header `sectionHeaderEnt` with index symidx -/
def getSymbolNameInSection 
  (elffile : RawELFFile) 
  (shte : RawSectionHeaderTableEntry)
  (sec : InterpretedSection)
  (symidx : Nat) 
  : Except String String :=
    let offset := symidx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry? (sec.section_body) is64Bit isBigEndian offset with
    | .error warn => .error warn
    | .ok ste => symbolNameByLinkAndOffset 
      elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste)
  where
    isBigEndian := ELFHeader.isBigendian $ elffile.getRawELFHeader
    is64Bit := ELFHeader.is64Bit $ elffile.getRawELFHeader

def printSymbolsForSectionType (elffile: RawELFFile) (ent_type : Nat) :=
  let ofType := elffile.getRawSectionHeaderTableEntries.filter $ λ⟨shte, _⟩↦
    SectionHeaderTableEntry.sh_type shte == ent_type
  ofType.forM $ λ⟨shte, sec⟩ ↦ printSymbolsForSection elffile shte sec

def printStringsForSectionIdx (elffile : RawELFFile) (idx : Nat) :=
  match elffile.getRawSectionHeaderTableEntries[idx]? with
  | .none => IO.println s!"There doesn't appear to be a section header {idx}"
  | .some ⟨_, sec⟩ => for byte in sec.section_body  do
      if byte == 0 then IO.print '\n' else IO.print (Char.ofNat byte.toNat)

def dumpBytesAsHex (bytes : ByteArray) : IO Unit := do
    let mut idx := 0
    for byte in bytes do
      if idx % 8 == 0 then do IO.print " "
      if idx % 16 == 0 then do IO.print "\n"
      IO.print $ (Nat.toDigits 16 (byte.toNat)) 
        |> (λl ↦ if l.length == 1 then '0' :: l else l)
        |> List.asString
      IO.print " "
      idx ← pure $ idx + 1

def printHexForSectionIdx (elffile : RawELFFile) (idx : Nat) :=
  match elffile.getRawSectionHeaderTableEntries[idx]? with
  | .none => IO.println s!"There doesn't appear to be a section header {idx}"
  | .some ⟨_, sec⟩ => dumpBytesAsHex sec.section_body

def printDynamics (elffile : RawELFFile) :=
  let dynamics := elffile.getRawSectionHeaderTableEntries.filter $ λsec ↦
    SectionHeaderTableEntry.sh_type sec.fst == ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC
  for ⟨shte, sec⟩ in dynamics do
    for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Dynamic Entry {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawDynamicEntry? 
      sec.section_body
      (ELFHeader.is64Bit elffile.getRawELFHeader) 
      (ELFHeader.isBigendian elffile.getRawELFHeader) 
      offset with
    | .error e => IO.println s!"warning: {e}"
    | .ok dynamicEnt => IO.println $ repr dynamicEnt

-- should use this for both SHT_NOTE sections and PT_NOTE segments
def printNotes
  (elffile: RawELFFile)
  (shte: RawSectionHeaderTableEntry)
  (sec: InterpretedSection)
  : IO Unit := recur (SectionHeaderTableEntry.sh_size shte) 0
  where
    isBigEndian := ELFHeader.isBigendian $ elffile.getRawELFHeader
    is64Bit := ELFHeader.is64Bit $ elffile.getRawELFHeader
    alignTo4 n := n + (n % 4)
    recur space offset := 
      match space with
      | 0 => pure ()
      | spaceminus + 1 => --we work with space-1 to automatically prove termination
        match mkRawNoteEntry? (sec.section_body) isBigEndian is64Bit offset with
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
      --eh bytes (SectionHeaderTableEntry.sh_offset sh) (SectionHeaderTableEntry.sh_size sh)

def printRelocationA 
  (elffile : RawELFFile)
  (shte : RawSectionHeaderTableEntry)
  (sec : InterpretedSection) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Relocation {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawRelocationA? sec.section_body is64Bit isBigEndian offset with
    | .error warn => IO.println warn
    | .ok ra =>
      let r_sym := if is64Bit then (RelocationA.ra_info ra) / 2^32 else (RelocationA.ra_info ra) / 2^8
      match elffile.getRawSectionHeaderTableEntries[linkedSymbolNames]? with
      | .some ⟨shte, sec⟩ => match getSymbolNameInSection elffile shte sec r_sym with
        | .ok name => IO.print s!"{name}\n"
        | .error warn => IO.print s!"??? - {warn}\n"
      | .none => IO.print s!"??? - can't locate string table of symbol names\n"
      IO.println $ repr ra
  where
    isBigEndian := ELFHeader.isBigendian $ elffile.getRawELFHeader
    is64Bit := ELFHeader.is64Bit $ elffile.getRawELFHeader
    linkedSymbolNames := SectionHeaderTableEntry.sh_link shte

def printRelocationSections (elffile: RawELFFile) :=
  let relocations := elffile.getRawSectionHeaderTableEntries.filter $ λsec ↦
    SectionHeaderTableEntry.sh_type sec.fst == ELFSectionHeaderTableEntry.Type.SHT_RELA
  for ⟨shte, sec⟩ in relocations do
      match sec.section_name_as_string with
      | .some name => IO.print s!"Relocations from {name}\n"
      | .none => IO.print s!"Relocations from unnamed section\n"
      printRelocationA elffile shte sec

def runReadCmd (p: Cli.Parsed): IO UInt32 := do
  
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
    | "file-header" => IO.println $ repr elfheader
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
      | none => IO.println "couldn't parse section number provided for string dump"
      | some idx => printStringsForSectionIdx elffile idx
    | "hex-dump" => match flag.as? Nat with
      | none => IO.println "couldn't parse section number provided for hex dump"
      | some idx => printHexForSectionIdx elffile idx
    | "notes" => printNoteSections elffile
    | "relocs" => printRelocationSections elffile
    | _ => IO.println $ "unrecognized flag: " ++ flag.flag.longName

  return 0
