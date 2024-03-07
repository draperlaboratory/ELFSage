
/- Lean doesn't seem to have fixed-width signed Ints in the stdlib. -/
structure SInt32 :=
  bytes : UInt32

structure SInt64 :=
  bytes : UInt64

abbrev elf64_half   := UInt16 --Half word
abbrev elf64_off    := UInt64 --Offsets
abbrev elf64_addr   := UInt64 --Addresses
abbrev elf64_word   := UInt32 --Unsigned words
abbrev elf64_sword  := SInt32 --Signed words
abbrev elf64_xword  := UInt64 --Extra wide words
abbrev elf64_sxword := UInt64 --Extra wide signed words
