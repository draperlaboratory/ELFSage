import Cli
import ELFSage.Util.Cli
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

def printProgramHeaders (eh : RawELFHeader) (bytes : ByteArray) :=
  for idx in [:ELFHeader.e_phnum eh] do
    IO.println s!"\nProgram Header {idx}\n"
    let offset := ELFHeader.e_phoff eh + (idx * ELFHeader.e_phentsize eh)
    match mkRawProgramHeaderTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error warn => IO.println warn
    | .ok programHeader => IO.println $ repr programHeader

-- TODO: sectionNameByOffset and symbolNameByLinkAndOffset should be unified and
-- put under Types.ELFFile. Should perhaps signify that the symbol name recovery
-- is a Unix/System V thing rather than an ELF thing: https://refspecs.linuxfoundation.org/elf/elf.pdf
def sectionNameByOffset (eh: RawELFHeader) (bytes : ByteArray) (offset : Nat) : Except String String := 
  let header_offset := ELFHeader.e_shoff eh + (ELFHeader.e_shstrndx eh * ELFHeader.e_shentsize eh)
  match mkRawSectionHeaderTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) header_offset with
  | .error _ => .error "unable to locate the section header table entry for the section names"
  | .ok sh => 
    if h : bytes.size < SectionHeaderTableEntry.sh_offset sh + SectionHeaderTableEntry.sh_size sh
    then .error "The section header for the string table of section names describes a section that overflows the end of the binary"
    else 
      let stringtable := mkELFStringTable bytes 
        (SectionHeaderTableEntry.sh_offset sh) 
        (SectionHeaderTableEntry.sh_size sh) 
        (by omega)
      pure $ stringtable.stringAt offset

def getSectionByIndex (eh: RawELFHeader) (bytes : ByteArray) (idx: Nat) : Except String RawSectionHeaderTableEntry :=
  let header_offset := ELFHeader.e_shoff eh + (idx * ELFHeader.e_shentsize eh)
  match mkRawSectionHeaderTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) header_offset with
  | .error warn => .error s!"unable to locate the section header table at index {idx}, {warn}."
  | .ok s => .ok s

def symbolNameByLinkAndOffset 
  (elfheader : RawELFHeader) 
  (bytes : ByteArray) 
  (linkIdx: Nat) 
  (offset : Nat) 
  : Except String String := do
  let sh ← getSectionByIndex elfheader bytes linkIdx
  if h : bytes.size < SectionHeaderTableEntry.sh_offset sh + SectionHeaderTableEntry.sh_size sh
  then .error "The section header that the symbol table references describes a section that overflows the end of the binary"
  else 
    let stringtable := mkELFStringTable bytes 
      (SectionHeaderTableEntry.sh_offset sh) 
      (SectionHeaderTableEntry.sh_size sh) 
      (by omega)
    pure $ stringtable.stringAt offset

def printSectionHeaders (eh: RawELFHeader) (bytes : ByteArray) :=
  for idx in [:ELFHeader.e_shnum eh] do
    IO.print s!"\nSection Header {idx}: "
    let offset := ELFHeader.e_shoff eh + (idx * ELFHeader.e_shentsize eh)
    match mkRawSectionHeaderTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error warn => IO.println warn
    | .ok sh => 
      match sectionNameByOffset eh bytes (SectionHeaderTableEntry.sh_name sh) with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}¬"
      IO.println $ repr sh

/- Prints all the symbols in the section with header `sectionHeaderEnt` -/
def printSymbolsForSection (eh: RawELFHeader) (bytes : ByteArray) (sh: RawSectionHeaderTableEntry) :=
  for idx in [:SectionHeaderTableEntry.sh_size sh / SectionHeaderTableEntry.sh_entsize sh] do
    IO.print s!"Symbol {idx}: "
    let offset := SectionHeaderTableEntry.sh_offset sh + (idx * SectionHeaderTableEntry.sh_entsize sh)
    match mkRawSymbolTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error warn => IO.println warn
    | .ok ste => 
      match symbolNameByLinkAndOffset eh bytes (SectionHeaderTableEntry.sh_link sh) (SymbolTableEntry.st_name ste) with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}\n"
      IO.println $ repr ste

/- gets the symbol in the section with header `sectionHeaderEnt` with index symidx -/
def getSymbolNameInSection 
  (eh: RawELFHeader) 
  (bytes : ByteArray) 
  (sh: RawSectionHeaderTableEntry) 
  (symidx : Nat) 
  : Except String String :=
    let offset := SectionHeaderTableEntry.sh_offset sh + (symidx * SectionHeaderTableEntry.sh_entsize sh)
    match mkRawSymbolTableEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error warn => .error warn
    | .ok ste => symbolNameByLinkAndOffset 
      eh bytes (SectionHeaderTableEntry.sh_link sh) (SymbolTableEntry.st_name ste)

def printSymbolsForSectionType (eh: RawELFHeader) (bytes : ByteArray) (ent_type : Nat) :=
  for idx in [:ELFHeader.e_shnum eh] do
    match getSectionByIndex eh bytes idx with
    | .error _ => pure ()
    | .ok sh =>

    if SectionHeaderTableEntry.sh_type sh != ent_type
    then pure ()
    else printSymbolsForSection eh bytes sh

def printStringsForSectionIdx (elfheader : RawELFHeader) (bytes : ByteArray) (idx : Nat) :=
  match getSectionByIndex elfheader bytes idx with
  | .error _ => IO.println s!"There doesn't appear to be a section header {idx}"
  | .ok sh =>

  if h : bytes.size < SectionHeaderTableEntry.sh_offset sh + SectionHeaderTableEntry.sh_size sh
  then IO.println "The requested section header {idx} describes a section that overflows the end of the binary"
  else
    let stringtable := mkELFStringTable bytes (SectionHeaderTableEntry.sh_offset sh) (SectionHeaderTableEntry.sh_size sh) (by omega)
    for byte in stringtable.strings do
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

def printHexForSectionIdx (elfheader : RawELFHeader) (bytes : ByteArray) (idx : Nat) :=
  match getSectionByIndex elfheader bytes idx with
  | .error _ => IO.println s!"There doesn't appear to be a section header {idx}"
  | .ok sh =>
  if bytes.size < SectionHeaderTableEntry.sh_offset sh + SectionHeaderTableEntry.sh_size sh
  then IO.println "The requested section header {idx} describes a section that overflows the end of the binary"
  else do
    let targetSection := bytes.extract 
      (SectionHeaderTableEntry.sh_offset sh) 
      (SectionHeaderTableEntry.sh_offset sh + SectionHeaderTableEntry.sh_size sh)
    dumpBytesAsHex targetSection

def printDynamics (eh: RawELFHeader) (bytes : ByteArray) :=
  for idx in [:ELFHeader.e_shnum eh] do
    match getSectionByIndex eh bytes idx with
    | .error _ => pure ()
    | .ok sh =>
    if SectionHeaderTableEntry.sh_type sh != ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC
    then pure ()
    else for idx in [:SectionHeaderTableEntry.sh_size sh / SectionHeaderTableEntry.sh_entsize sh] do
    IO.print s!"Dynamic Entry {idx}: "
    let offset := SectionHeaderTableEntry.sh_offset sh + (idx * SectionHeaderTableEntry.sh_entsize sh)
    match mkRawDynamicEntry? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error e => IO.println s!"warning: {e}"
    | .ok dynamicEnt => IO.println $ repr dynamicEnt

-- should use this for both SHT_NOTE sections and PT_NOTE segments
def printNotes
  (eh: RawELFHeader)
  (bytes : ByteArray)
  (offset : Nat)
  (space : Nat)
  : IO Unit := match space with
  | 0 => pure ()
  | spaceminus + 1 => --we work with space-1 to automatically prove termination
    match mkRawNoteEntry? bytes (ELFHeader.isBigendian eh) (ELFHeader.is64Bit eh) offset with
    | .error e => IO.println e
    | .ok ne => do
      IO.println $ repr ne
      let notesize := 0xc + alignTo4 (NoteEntry.note_name ne).size + alignTo4 (NoteEntry.note_desc ne).size
      if spaceminus - notesize ≥ 0xb
      then printNotes eh bytes (offset + notesize) (spaceminus - (notesize - 1))
      else pure ()
  where 
    alignTo4 n := n + (n % 4)

def printNoteSections (eh: RawELFHeader) (bytes : ByteArray) :=
  for idx in [:ELFHeader.e_shnum eh] do
    match getSectionByIndex eh bytes idx with
    | .error _ => pure ()
    | .ok sh =>
    if SectionHeaderTableEntry.sh_type sh == ELFSectionHeaderTableEntry.Type.SHT_NOTE then
      match sectionNameByOffset eh bytes (SectionHeaderTableEntry.sh_name sh) with
      | .ok name => IO.print s!"Notes from {name}\n"
      | .error warn => IO.print s!"Notes from ??? - {warn}\n"
      printNotes eh bytes (SectionHeaderTableEntry.sh_offset sh) (SectionHeaderTableEntry.sh_size sh)

def printRelocationA 
  (eh : RawELFHeader) 
  (bytes: ByteArray) 
  (sh: RawSectionHeaderTableEntry) :=
  for idx in [:SectionHeaderTableEntry.sh_size sh / SectionHeaderTableEntry.sh_entsize sh] do
    IO.print s!"Relocation {idx}: "
    let offset := SectionHeaderTableEntry.sh_offset sh + (idx * SectionHeaderTableEntry.sh_entsize sh)
    match mkRawRelocationA? bytes (ELFHeader.is64Bit eh) (ELFHeader.isBigendian eh) offset with
    | .error warn => IO.println warn
    | .ok ra =>
      let r_sym := if ELFHeader.is64Bit eh then (RelocationA.ra_info ra) / 2^32 else (RelocationA.ra_info ra) / 2^8
      let linkedSection := getSectionByIndex eh bytes (SectionHeaderTableEntry.sh_link sh)
      match linkedSection with
      | .ok linkedSection => match getSymbolNameInSection eh bytes linkedSection r_sym with
        | .ok name => IO.print s!"{name}\n"
        | .error warn => IO.print s!"??? - {warn}\n"
      | .error warn => IO.print s!"??? - can't locate names: {warn}\n"
      IO.println $ repr ra

def printRelocationSections (eh: RawELFHeader) (bytes : ByteArray) :=
  for idx in [:ELFHeader.e_shnum eh] do
    match  getSectionByIndex eh bytes idx with
    | .error _ => pure ()
    | .ok sh =>
    if SectionHeaderTableEntry.sh_type sh == ELFSectionHeaderTableEntry.Type.SHT_RELA then
      match sectionNameByOffset eh bytes (SectionHeaderTableEntry.sh_name sh) with
      | .ok name => IO.print s!"Relocations from {name}\n"
      | .error warn => IO.print s!"Relocations from ??? - {warn}\n"
      printRelocationA eh bytes sh

def runReadCmd (p: Cli.Parsed): IO UInt32 := do
  
  match checkImplemented p with
  | .error warn => IO.println warn *> return 1
  | .ok _ => do

  if p.flags.size == 0 then do
    p.printHelp
    return 1

  let targetBinary := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  match mkRawELFHeader? bytes with
  | .error warn => IO.println warn *> return 1
  | .ok elfheader => do

  for flag in p.flags do
    match flag.flag.longName with
    | "file-header" => IO.println $ repr elfheader
    | "headers" => do
      IO.println $ repr elfheader
      printProgramHeaders elfheader bytes
      printSectionHeaders elfheader bytes
    | "program-headers" => printProgramHeaders elfheader bytes
    | "segments" => printProgramHeaders elfheader bytes
    | "section-headers" => printSectionHeaders elfheader bytes
    | "sections" => printSectionHeaders elfheader bytes
    | "dynamic" => printDynamics elfheader bytes
    | "dyn-syms" => 
      let type := ELFSectionHeaderTableEntry.Type.SHT_DYNSYM;
      printSymbolsForSectionType elfheader bytes type
    | "syms" => 
      let symtab := ELFSectionHeaderTableEntry.Type.SHT_SYMTAB
      printSymbolsForSectionType elfheader bytes symtab
      --TODO fallback to DYNSYM when SYMTAB isn't present
    | "string-dump" => match flag.as? Nat with
      | none => IO.println "couldn't parse section number provided for string dump"
      | some idx => printStringsForSectionIdx elfheader bytes idx
    | "hex-dump" => match flag.as? Nat with
      | none => IO.println "couldn't parse section number provided for hex dump"
      | some idx => printHexForSectionIdx elfheader bytes idx
    | "notes" => printNoteSections elfheader bytes
    | "relocs" => printRelocationSections elfheader bytes
    | _ => IO.println $ "unrecognized flag: " ++ flag.flag.longName

  return 0
