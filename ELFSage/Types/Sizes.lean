/- Lean doesn't seem to have fixed-width signed Ints in the stdlib. -/
structure SInt32 := bytes : UInt32

def SInt32.toInt (si :SInt32) : Int := 
  let uval := si.bytes.toNat
  if uval < 2 ^ 31 then ↑uval
  else (↑uval : Int) - (2 ^ 32)

structure SInt64 := bytes : UInt64

def SInt64.toInt (si :SInt64) : Int := 
  let uval := si.bytes.toNat
  if uval < 2 ^ 63 then ↑uval
  else (↑uval : Int) - (2 ^ 64)

/-- ELF64 Half word -/
abbrev elf64_half   := UInt16 
/-- ELF64 Offset size -/
abbrev elf64_off    := UInt64 
/-- ELF64 Addresse size -/
abbrev elf64_addr   := UInt64 
/-- ELF64 Unsigned word size -/
abbrev elf64_word   := UInt32
/-- ELF64 Signed word size -/
abbrev elf64_sword  := SInt32
/-- ELF64 Extra wide word size -/
abbrev elf64_xword  := UInt64
/-- ELF64 Extra wide signed word size -/
abbrev elf64_sxword := SInt64

/-- ELF32 Half word -/
abbrev elf32_half   := UInt16 
/-- ELF32 Offset size -/
abbrev elf32_off    := UInt32 
/-- ELF32 Addresse size -/
abbrev elf32_addr   := UInt32
/-- ELF32 Unsigned word size -/
abbrev elf32_word   := UInt32
/-- ELF32 Signed word size -/
abbrev elf32_sword  := SInt32
