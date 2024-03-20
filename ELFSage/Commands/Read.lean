import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Constants.SectionHeaderTable
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.SymbolTable
import ELFSage.Types.StringTable
import ELFSage.Types.Dynamic

def checkImplemented (p: Cli.Parsed) : Except String Unit := do
  let unimplemented := 
    [ "a", "all"
    , "g", "section-groups"
    , "t", "section-details"
    , "lto-syms"
    , "sym-base"
    , "C", "demangle"
    , "n", "notes"
    , "r", "relocs"
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

def printProgramHeaders (elfheader : RawELFHeader) (bytes : ByteArray) :=
  for idx in [:elfheader.phnum] do
    IO.println s!"\nProgram Header {idx}\n"
    let offset := elfheader.phoff + (idx * elfheader.phentsize)
    match mkRawProgramHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error warn => IO.println warn
    | .ok programHeader => IO.println $ repr programHeader

-- TODO: sectionNameByOffset and symbolNameByLinkAndOffset should be unified and
-- put under Types.ELFFile. Should perhaps signify that the symbol name recovery
-- is a Unix/System V thing rather than an ELF thing: https://refspecs.linuxfoundation.org/elf/elf.pdf
def sectionNameByOffset (elfheader : RawELFHeader) (bytes : ByteArray) (offset : Nat) : Except String String := 
  let header_offset := elfheader.shoff + (elfheader.shstrndx * elfheader.shentsize)
  match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian header_offset with
  | .error _ => .error "unable to locate the section header table entry for the section names"
  | .ok sectionHeader => 
    if h : bytes.size < sectionHeader.sh_offset + sectionHeader.sh_size 
    then .error "The section header for eh_shstrndx describes a section that overflows the end of the binary"
    else 
      let stringtable := mkELFStringTable bytes sectionHeader.sh_offset sectionHeader.sh_size (by omega)
      pure $ stringtable.stringAt offset

def symbolNameByLinkAndOffset (elfheader : RawELFHeader) (bytes : ByteArray) (linkIdx: Nat) (offset : Nat) : Except String String := 
  let header_offset := elfheader.shoff + (linkIdx * elfheader.shentsize)
  match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian header_offset with
  | .error _ => .error "unable to locate the section header table referenced by this symbol table"
  | .ok sectionHeader => 
    if h : bytes.size < sectionHeader.sh_offset + sectionHeader.sh_size 
    then .error "The section header that the symbol table references describes a section that overflows the end of the binary"
    else 
      let stringtable := mkELFStringTable bytes sectionHeader.sh_offset sectionHeader.sh_size (by omega)
      pure $ stringtable.stringAt offset

def printSectionHeaders (elfheader : RawELFHeader) (bytes : ByteArray) :=
  for idx in [:elfheader.shnum] do
    IO.print s!"\nSection Header {idx}: "
    let offset := elfheader.shoff + (idx * elfheader.shentsize)
    match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error warn => IO.println warn
    | .ok sectionHeader => 
      match sectionNameByOffset elfheader bytes sectionHeader.sh_name with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}¬"
      IO.println $ repr sectionHeader

def printSymbolsForSection (elfheader : RawELFHeader) (bytes : ByteArray) (sectionHeaderEnt : RawSectionHeaderTableEntry) :=
  for idx in [:sectionHeaderEnt.sh_size / sectionHeaderEnt.sh_entsize] do
    IO.print s!"Symbol {idx}: "
    let offset := sectionHeaderEnt.sh_offset + (idx * sectionHeaderEnt.sh_entsize)
    match mkRawSymbolTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error warn => IO.println warn
    | .ok symbolTableEnt => 
      match symbolNameByLinkAndOffset elfheader bytes sectionHeaderEnt.sh_link symbolTableEnt.st_name with
      | .ok name => IO.print s!"{name}\n"
      | .error warn => IO.print s!"??? - {warn}¬"
      IO.println $ repr symbolTableEnt

def printSymbolsForSectionType (elfheader : RawELFHeader) (bytes : ByteArray) (ent_type : Nat) :=
  for idx in [:elfheader.shnum] do
    let offset := elfheader.shoff + (idx * elfheader.shentsize)
    match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error _ => pure ()
    | .ok sectionHeaderEnt =>

    if sectionHeaderEnt.sh_type != ent_type
    then pure ()
    else printSymbolsForSection elfheader bytes sectionHeaderEnt

def printStringsForSectionIdx (elfheader : RawELFHeader) (bytes : ByteArray) (idx : Nat) :=
  let offset := elfheader.shoff + (idx * elfheader.shentsize)
  match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
  | .error _ => IO.println s!"There doesn't appear to be a section header {idx}"
  | .ok sectionHeader =>

  if h : bytes.size < sectionHeader.sh_offset + sectionHeader.sh_size 
  then IO.println "The requested section header {idx} describes a section that overflows the end of the binary"
  else
    let stringtable := mkELFStringTable bytes sectionHeader.sh_offset sectionHeader.sh_size (by omega)
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
  let offset := elfheader.shoff + (idx * elfheader.shentsize)
  match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
  | .error _ => IO.println s!"There doesn't appear to be a section header {idx}"
  | .ok sectionHeader =>
  if bytes.size < sectionHeader.sh_offset + sectionHeader.sh_size 
  then IO.println "The requested section header {idx} describes a section that overflows the end of the binary"
  else do
    let targetSection := bytes.extract sectionHeader.sh_offset (sectionHeader.sh_offset + sectionHeader.sh_size)
    dumpBytesAsHex targetSection

def printDynamics (elfheader : RawELFHeader) (bytes : ByteArray) :=
  for idx in [:elfheader.shnum] do
    let offset := elfheader.shoff + (idx * elfheader.shentsize)
    match mkRawSectionHeaderTableEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error _ => pure ()
    | .ok sectionHeaderEnt =>
    if sectionHeaderEnt.sh_type != ELFSectionHeaderTableEntry.Type.SHT_DYNAMIC
    then pure ()
    else for idx in [:sectionHeaderEnt.sh_size / sectionHeaderEnt.sh_entsize] do
    IO.print s!"Dynamic Entry {idx}: "
    let offset := sectionHeaderEnt.sh_offset + (idx * sectionHeaderEnt.sh_entsize)
    match mkRawDynamicEntry? bytes elfheader.is64Bit elfheader.isBigendian offset with
    | .error e => IO.println s!"warning: {e}"
    | .ok dynamicEnt => IO.println $ repr dynamicEnt


def runReadCmd (p: Cli.Parsed): IO UInt32 := do
  
  match checkImplemented p with
  | .error warn => IO.println warn *> return 1
  | .ok _ => do

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
    | _ => IO.println $ "unrecognized flag: " ++ flag.flag.longName

  return 0
