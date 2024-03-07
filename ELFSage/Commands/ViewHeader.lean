import Cli
import ELFSage.Util.Cli
import ELFSage.Types.ELFHeader
import ELFSage.Types.ProgramHeaderTable

def runViewHeaderCmd (p : Cli.Parsed): IO UInt32 := do
  let targetBinary : System.FilePath := (p.positionalArg! "targetBinary").as! System.FilePath
  let bytes ← IO.FS.readBinFile targetBinary
  if badinput₁ : bytes.size < 64 then
    IO.println "File doesn't appear to be an ELF Binary. It's too small!"
    return 1
  else
  let header := mkELF64Header bytes (Nat.not_lt.mp badinput₁)
  if h₂ : bytes.size < header.elf64_phoff.toNat + header.elf64_phnum.toNat * header.elf64_phentsize.toNat then
    IO.println "Something's wrong. There's not enough room for the program headers."
    return 1
  else if fits₂ : header.elf64_phentsize.toNat < 56 then
    IO.println $
      s! "Something's wrong. The ELF header says phentsize is {header.elf64_phentsize.toNat} bytes," 
      ++ " but you need at least 56 bytes to fit everything in there."
    return 1
  else
    IO.println $ repr header
    for h₃ : idx in [0:header.elf64_phnum.toNat] do
      have fits : bytes.size - (header.elf64_phoff.toNat + idx * header.elf64_phentsize.toNat) ≥ 56 := by
        have fits₀ : (idx + 1) * header.elf64_phentsize.toNat ≤ header.elf64_phnum.toNat * header.elf64_phentsize.toNat := by
          apply Nat.mul_le_mul_right
          exact h₃.right
        have fits₁ : idx * header.elf64_phentsize.toNat + header.elf64_phentsize.toNat ≤ header.elf64_phnum.toNat * header.elf64_phentsize.toNat := by
          rw [Nat.mul_comm]
          rw [←Nat.mul_succ]
          rw [Nat.mul_comm]
          assumption
        omega
      let entry := mkELF64ProgramHeaderTableEntry bytes (header.elf64_phoff.toNat + idx * header.elf64_phentsize.toNat) True fits
      IO.println s!"\nprogram header {idx}:\n"
      IO.println $ repr entry
    return 0
