import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable
import ELFSage.Types.SectionHeaderTable

def runViewHeaderCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary : System.FilePath := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary
  if haveEHSpace : bytes.size < 64 then
    IO.println "File doesn't appear to be an ELF Binary. It's too small!"
    return 1
  else

  --XXX we probably want a uniform asNat interface for 64/32 bit ELF headers, etc
  let header := mkELF64Header bytes (by omega)
  let phoff  := header.elf64_phoff.toNat
  let phentsize  := header.elf64_phentsize.toNat
  let phnum  := header.elf64_phnum.toNat
  let shoff  := header.elf64_shoff.toNat
  let shentsize  := header.elf64_shentsize.toNat
  let shnum  := header.elf64_shnum.toNat

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
    IO.println $ repr header
    let phentries := List.reverse $ 
      bytes.getEntriesFrom phoff phnum phentsize 56 (by omega) (by omega) (mkELF64ProgramHeaderTableEntry True bytes)
    let mut phidx ← pure 0
    for ent in phentries do
      phidx ← pure $ phidx + 1
      IO.println $ s!"\nProgram Header {phidx}\n"
      IO.println $ repr ent
    let shentries := List.reverse $ 
      bytes.getEntriesFrom shoff shnum shentsize 64 (by omega) (by omega) (mkELF64SectionHeaderTableEntry True bytes)
    let mut shidx ← pure 0
    for ent in shentries do
      shidx ← pure $ shidx + 1
      IO.println $ s!"\nSection Header {shidx}\n"
      IO.println $ repr ent
    return 0
