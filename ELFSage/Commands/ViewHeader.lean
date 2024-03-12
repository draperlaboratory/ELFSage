import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader
import ELFSage.Types.ELF32Header
import ELFSage.Types.ELF64Header
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.SectionHeaderTable
import ELFSage.Types.SymbolTable
import ELFSage.Types.StringTable

def ByteArray.printSymbolTableFor 
  (bs : ByteArray)
  (isBigendian : Bool)
  (shent : ELF64SectionHeaderTableEntry)
  : IO Bool
  := do
    let sh_size := shent.sh_size.toNat
    let sh_entsize := shent.sh_entsize.toNat
    let sh_offset  := shent.sh_offset.toNat
    let sym_count : Nat := sh_size / sh_entsize

    if haveSHEntSpace : sh_entsize < 24 then
      IO.println $
        s! "Something's wrong. A section header entry indicates a symbol table with an sh_entsize of {sh_entsize}" 
        ++ "but you need at least 24 bytes to fit a symbol"
      return False
    else if haveSTSpace : bs.size < sh_offset + sym_count * sh_entsize then
      IO.println $
        s! "Something's wrong. There's not enough room for a symbol table's entries"
      return False
    else
      let symbols := bs.getEntriesFrom sh_offset sym_count sh_entsize 0x18 (by omega) (by omega) (mkELF64SymbolTableEntry isBigendian bs)
      let mut symidx ← pure 0
      for symbol in symbols do
        symidx ← pure $ symidx + 1
        IO.println s!"symbol {symidx}"
        IO.println $ repr symbol
      return True

def ByteArray.printStringTableFor
  (bs : ByteArray)
  (stent : ELF64SectionHeaderTableEntry)
  : IO Bool := do
  let sh_size := stent.sh_size.toNat
  let sh_offset  := stent.sh_offset.toNat

  if haveSTSpace : bs.size < sh_offset + sh_size then
    IO.println $
      s! "Something's wrong. A section header entry indicates a {sh_size} byte string table"
      ++ s! " at offset {sh_offset}. But the binary is only {bs.size} bytes, so that won't fit."
    return False
  else
    let stringTable := mkELFStringTable bs sh_offset sh_size (by omega)
    for byte in stringTable do
      if byte == 0 then IO.print '\n' else IO.print (Char.ofNat byte.toNat)
    return True

def runViewHeaderCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary : System.FilePath := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary

  if haveEHSpace : bytes.size < 64 then
    IO.println "File doesn't appear to be an ELF Binary. It's too small!"
    return 0
  else

  --XXX we probably want a uniform asNat interface for 64/32 bit ELF headers, etc
  let elfheader := mkELF64Header bytes (by omega)
  let phoff     := elfheader.phoff.toNat
  let phentsize := elfheader.phentsize.toNat
  let phnum     := elfheader.phnum.toNat
  let shoff     := elfheader.shoff.toNat
  let shentsize := elfheader.shentsize.toNat
  let shnum     := elfheader.shnum.toNat
  let isBigendian := ELFIdent.isBigendian elfheader

  if havePHSpace : bytes.size < phoff + phnum * phentsize then
    IO.println "Something's wrong. There's not enough room for the program header table."
    return 1
  else if haveSHSpace : bytes.size < shoff + shnum * shentsize then
    IO.println "Something's wrong. There's not enough room for the section header table."
    return 1
  else if havePHEntSpace : phentsize < 56 then
    IO.println $
      s! "Something's wrong. The ELF header says phentsize is {phentsize} bytes," 
      ++ " but you need at least 56 bytes to fit everything in there."
    return 1
  else if haveSHEntSpace : shentsize < 64 then
    IO.println $
      s! "Something's wrong. The ELF header says shentsize is {shentsize} bytes," 
      ++ " but you need at least 64 bytes to fit everything in there."
    return 1
  else
    IO.println $ repr elfheader

    let phentries := bytes.getEntriesFrom phoff phnum phentsize 56 (by omega) (by omega) (mkELF64ProgramHeaderTableEntry isBigendian bytes)
    let mut phidx ← pure 0
    for ent in phentries do
      phidx ← pure $ phidx + 1
      IO.println $ s!"\nProgram Header {phidx}\n"
      IO.println $ repr ent

    let shentries := bytes.getEntriesFrom shoff shnum shentsize 64 (by omega) (by omega) (mkELF64SectionHeaderTableEntry isBigendian bytes)
    let mut shidx ← pure 0
    for ent in shentries do
      shidx ← pure $ shidx + 1
      IO.println $ s!"\nSection Header {shidx}\n"
      IO.println $ repr ent

    let symtabs := shentries.filter (λshe => she.sh_type ∈ [SHT_SYMTAB, SHT_DYNSYM])
    let mut symtabidx ← pure 0
    for ent in symtabs do
      symtabidx ← pure $ symtabidx + 1
      IO.println $ s!"\nSymbol Table {symtabidx}\n"
      let val ← bytes.printSymbolTableFor isBigendian ent
      if !val then return 1

    let strtabs := shentries.filter (λshe => she.sh_type ∈ [SHT_STRTAB])
    let mut strtabidx ← pure 0
    for ent in strtabs do
      strtabidx ← pure $ symtabidx + 1
      IO.println $ s!"\nString Table {strtabidx}\n"
      let val ← bytes.printStringTableFor ent
      if !val then return 1

    return 0
