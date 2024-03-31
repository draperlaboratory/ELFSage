import Cli
import ELFSage.Util.Cli
import ELFSage.Util.Flags
import ELFSage.Util.Hex
import ELFSage.Util.List
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

open Hex
open Flags

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

def printFileHeader (eh : RawELFHeader) := do
  IO.println $ toString eh

private def getProgramHeaderType (n: UInt32) := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 1           => "PT_LOAD"          -- (0x1)
  | 2           => "PT_DYNAMIC"       -- (0x2)
  | 3           => "PT_INTERP"        -- (0x3)
  | 4           => "PT_NOTE"          -- (0x4)
  | 6           => "PT_PHDR"          -- (0x6)
  | 1685382480  => "PT_GNU_EH_FRAME"  -- (0x6474E550)
  | 1685382481  => "PT_GNU_STACK"     -- (0x6474E551)
  | 1685382482  => "PT_GNU_RELRO"     -- (0x6474E552)
  | 1685382483  => "PT_GNU_PROPERTY"  -- (0x6474E553)
  | _ => panic s!"Unrecognized program header type {n}"

private def getProgramHeaderFlag (flagIndex: Nat) := match flagIndex with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0  => "PF_X"  -- (0x1)
  | 1  => "PF_W"  -- (0x2)
  | 2  => "PF_R"  -- (0x4)
  | _ => panic s!"Unrecognized program header flag {flagIndex}"

private def programHeaderFlagsToString (flags: UInt32) (indent: String) : String :=
  getFlagBits flags.toNat 32
    |> .map (λ flag => s!"{indent}{getProgramHeaderFlag flag} (0x{toHex (1 <<< flag)})\n")
    |> List.insertionSort (. < .) -- sort by flag name
    |> String.join

def printProgramHeaders (elffile : RawELFFile) := do
  let mut out := "ProgramHeaders [\n"
  let headers := elffile.getRawProgramHeaderTableEntries
  let mut idx := 0
  for ⟨phte, _⟩ in headers do
    let nextHeader := match phte with
    | .elf32 ph => toString $ repr ph
    | .elf64 ph =>
        "  ProgramHeader {\n" ++
      s!"    Type: {getProgramHeaderType ph.p_type} (0x{toHex ph.p_type.toNat})\n" ++
      s!"    Offset: 0x{toHex ph.p_offset.toNat}\n" ++
      s!"    VirtualAddress: 0x{toHex ph.p_vaddr.toNat}\n" ++
      s!"    PhysicalAddress: 0x{toHex ph.p_paddr.toNat}\n" ++
      s!"    FileSize: {ph.p_filesz}\n" ++
      s!"    MemSize: {ph.p_memsz}\n" ++
      s!"    Flags [ (0x{toHex ph.p_flags.toNat})\n" ++
      s!"{     programHeaderFlagsToString ph.p_flags "      "}" ++
        "    ]\n" ++
      s!"    Alignment: {ph.p_align}\n" ++
        "  }\n"
    out := out ++ nextHeader
    idx := idx + 1

  out := out ++ "]"
  IO.println out

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

private def getSectionHeaderType (n: UInt32) := match n with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0           => "SHT_NULL"         -- (0x0)
  | 1           => "SHT_PROGBITS"     -- (0x1)
  | 3           => "SHT_STRTAB"       -- (0x3)
  | 4           => "SHT_RELA"         -- (0x4)
  | 6           => "SHT_DYNAMIC"      -- (0x6)
  | 7           => "SHT_NOTE"         -- (0x7)
  | 8           => "SHT_NOBITS"       -- (0x8)
  | 11          => "SHT_DYNSYM"       -- (0xB)
  | 14          => "SHT_INIT_ARRAY"   -- (0xE)
  | 15          => "SHT_FINI_ARRAY"   -- (0xF)
  | 1879048182  => "SHT_GNU_HASH"     -- (0x6FFFFFF6)
  | 1879048190  => "SHT_GNU_verneed"  -- (0x6FFFFFFE)
  | 1879048191  => "SHT_GNU_versym"   -- (0x6FFFFFFF)
  | _ => panic s!"Unrecognized section header type {n}"

private def getSectionHeaderFlag (flagIndex: Nat) := match flagIndex with
  -- TODO: Finish populating this list of magic numbers, it is incomplete.
  | 0  => "SHF_WRITE"      -- (0x1)
  | 1  => "SHF_ALLOC"      -- (0x2)
  | 2  => "SHF_EXECINSTR"  -- (0x4)
  | 4  => "SHF_MERGE"      -- (0x10)
  | 5  => "SHF_STRINGS"    -- (0x20)
  | _ => panic s!"Unrecognized section header flag {flagIndex}"

private def sectionHeaderFlagsToString (flags: UInt64) (indent: String) : String :=
  getFlagBits flags.toNat 64
    |> .map (λ flag => s!"{indent}{getSectionHeaderFlag flag} (0x{toHex (1 <<< flag)})\n")
    |> List.insertionSort (. < .) -- sort by flag name
    |> String.join

def printSectionHeaders (elffile : RawELFFile) := do
  let mut out := "Sections [\n"
  let headers := elffile.getRawSectionHeaderTableEntries
  let mut idx := 0
  for ⟨phte, sec⟩ in headers do
    let name := match sec.section_name_as_string with | .some s => s | _ => ""
    let nextHeader := match phte with
    | .elf32 sh => toString $ repr sh
    | .elf64 sh =>
        "  Section {\n" ++
      s!"    Index: {idx}\n" ++
      s!"    Name: {name} ({sh.sh_name})\n" ++
      s!"    Type: {getSectionHeaderType sh.sh_type} (0x{toHex sh.sh_type.toNat})\n" ++
      s!"    Flags [ (0x{toHex sh.sh_flags.toNat})\n" ++
      s!"{     sectionHeaderFlagsToString sh.sh_flags "      "}" ++
        "    ]\n" ++
      s!"    Address: 0x{toHex sh.sh_addr.toNat}\n" ++
      s!"    Offset: 0x{toHex sh.sh_offset.toNat}\n" ++
      s!"    Size: {sh.sh_size}\n" ++
      s!"    Link: {sh.sh_link}\n" ++
      s!"    Info: {sh.sh_info}\n" ++
      s!"    AddressAlignment: {sh.sh_addralign}\n" ++
      s!"    EntrySize: {sh.sh_entsize}\n" ++
        "  }\n"
    out := out ++ nextHeader
    idx := idx + 1

  out := out ++ "]"
  IO.println out

def printHeaders (elffile : RawELFFile) := do
  printFileHeader elffile.getRawELFHeader
  printSectionHeaders elffile
  printProgramHeaders elffile

/- Prints all the symbols in the section with header `sectionHeaderEnt` -/
def printSymbolsForSection
  (elffile : RawELFFile)
  (shte: RawSectionHeaderTableEntry)
  (sec : InterpretedSection) :=
  for idx in [:SectionHeaderTableEntry.sh_size shte / SectionHeaderTableEntry.sh_entsize shte] do
    IO.print s!"Symbol {idx}: "
    let offset := idx * SectionHeaderTableEntry.sh_entsize shte
    match mkRawSymbolTableEntry?
      sec.section_body
      (ELFHeader.is64Bit elffile)
      (ELFHeader.isBigendian elffile)
      offset
    with
    | .error warn => IO.println warn
    | .ok ste =>
      match symbolNameByLinkAndOffset elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste) with
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
      (sec.section_body)
      (ELFHeader.is64Bit elffile)
      (ELFHeader.isBigendian elffile)
      offset
    with
    | .error warn => .error warn
    | .ok ste => symbolNameByLinkAndOffset
      elffile (SectionHeaderTableEntry.sh_link shte) (SymbolTableEntry.st_name ste)

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
          (sec.section_body)
          (ELFHeader.isBigendian elffile)
          (ELFHeader.is64Bit elffile)
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
      (sec.section_body)
      (ELFHeader.is64Bit elffile)
      (ELFHeader.isBigendian elffile)
      offset
    with
    | .error warn => IO.println warn
    | .ok ra =>
      let r_sym := if (ELFHeader.is64Bit elffile)
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
      | none => IO.println "couldn't parse section number provided for string dump"
      | some idx => printStringsForSectionIdx elffile idx
    | "hex-dump" => match flag.as? Nat with
      | none => IO.println "couldn't parse section number provided for hex dump"
      | some idx => printHexForSectionIdx elffile idx
    | "notes" => printNoteSections elffile
    | "relocs" => printRelocationSections elffile
    | _ => IO.println $ "unrecognized flag: " ++ flag.flag.longName

  return 0
