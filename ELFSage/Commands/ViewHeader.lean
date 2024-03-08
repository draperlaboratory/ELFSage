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
    --DRY THIS
    for h₃ : idx in [0:phnum] do
      have fits : bytes.size - (phoff + idx * phentsize) ≥ 56 := by
        have fits₀ : (idx + 1) * phentsize ≤ phnum * phentsize := by
          apply Nat.mul_le_mul_right
          exact h₃.right
        have fits₁ : idx * phentsize + phentsize ≤ phnum * phentsize := by
          rw [Nat.mul_comm]
          rw [←Nat.mul_succ]
          rw [Nat.mul_comm]
          assumption
        omega
      let entry := mkELF64ProgramHeaderTableEntry True bytes (phoff + idx * phentsize) fits
      IO.println s!"\nprogram header {idx}:\n"
      IO.println $ repr entry
    for h₃ : idx in [0:shnum] do
      have fits : bytes.size - (shoff + idx * shentsize) ≥ 64 := by
        have fits₀ : (idx + 1) * shentsize ≤ shnum * shentsize := by
          apply Nat.mul_le_mul_right
          exact h₃.right
        have fits₁ : idx * shentsize + shentsize ≤ shnum * shentsize := by
          rw [Nat.mul_comm]
          rw [←Nat.mul_succ]
          rw [Nat.mul_comm]
          assumption
        omega
      let entry := mkELF64SectionHeaderTableEntry True bytes (shoff + idx * shentsize) fits
      IO.println s!"\nsection header {idx}:\n"
      IO.println $ repr entry
    return 0
